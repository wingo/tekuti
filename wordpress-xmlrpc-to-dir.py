#!/usr/bin/env python

import os
import pycurl
import string
import sys
import tempfile
import time
import urllib
import urlparse
import xmlrpclib

from HTMLParser import HTMLParser

class WordPress:

    def __init__(self, url, username, password):
        self.__username = username
        self.__password = password
        self.__proxy = xmlrpclib.ServerProxy(url)

    def all_posts(self):
        MAX_POST_NO = 10000
        return self.__proxy.metaWeblog.getRecentPosts("",
                                                      self.__username,
                                                      self.__password,
                                                      MAX_POST_NO)

    def post_comments(self, post):
        return self.__proxy.wp.getComments("",
                                           self.__username,
                                           self.__password,
                                           {"post_id" : post["postid"]})

class MyHTMLParser(HTMLParser):

    def __init__(self, url, imgdir):
        HTMLParser.__init__(self)
        self.url = url
        self.imgdir = imgdir

    def handle_starttag(self, tag, attrs):
        if tag == "img":
            for attr in attrs:
                if attr[0] == "src":
                    self.download_image(attr[1])

    def download_image(self, url):
        parsed = urlparse.urlparse(url)
        if parsed.netloc == self.url:
            print "  downloading %s" % url
            filename = os.path.join(self.imgdir, parsed.path[1:])
            try:
                os.makedirs(os.path.dirname(filename))
            except os.error:
                pass
            fp = open(filename, "wb")
            curl = pycurl.Curl()
            curl.setopt(pycurl.URL, url)
            curl.setopt(pycurl.FOLLOWLOCATION, 1)
            curl.setopt(pycurl.MAXREDIRS, 5)
            curl.setopt(pycurl.CONNECTTIMEOUT, 30)
            curl.setopt(pycurl.TIMEOUT, 300)
            curl.setopt(pycurl.NOSIGNAL, 1)
            curl.setopt(pycurl.WRITEDATA, fp)
            curl.perform()
            curl.close()
            fp.close()

def make_dir(path):
    os.mkdir(path)
    return path + "/"

def write_file(path, content):
    f = open(path, "w")
    f.write(content.encode("utf-8"))
    f.close()

def write_comment(comment, dir):
    def make_metadata():
        out = ""
        keys = {"comment_id" : "id",
                "author" : "author",
                "author_email" : "author_email",
                "author_url" : "author_url",
                "author_ip" : "author_ip",
                "status" : "approved"}
        for k in keys:
            if (k != "status") or ((k == "status") and (comment[k] != "spam")):
                out += "%s: %s\n" % (k, comment[k])
        date = comment["date_created_gmt"]
        out += "timestamp: %s\n" % int(time.mktime(date.timetuple()))
        return out

    write_file(dir + str(comment["comment_id"]),
               make_metadata() + "\n" + comment["content"])

def unescape(s):
    s = s.replace("&lt;", "<")
    s = s.replace("&gt;", ">")
    s = s.replace("&amp;", "&")
    return s

def make_post_key(post):
    d = post["dateCreated"].timetuple()
    pre = "%d/%02d/%02d/%s" % (d.tm_year, d.tm_mon, d.tm_mday, post["wp_slug"])
    return urllib.quote(pre, "").lower()

def get_post_images(post, url, imgdir):
    parser = MyHTMLParser(url, imgdir)
    parser.feed(post["description"])

def write_post(post, categories, comments, images_url, new_images_url):
    print "writing post %s" % unescape(post["title"])
    def make_metadata():
        out = ""
        keys = {"postid" : "id",
                "wp_author_display_name" : "author",
                "title" : "title",
                "post_status" : "status",
                "mt_allow_comments" : "comment_status"}
        for k in keys:
            value = post[k]
            if k == "mt_allow_comments":
                if value == 1:
                    value = "open"
                else:
                    value = "closed"
            out += "%s: %s\n" % (keys[k], value)
        out += "tags: %s\n" % ", ".join(categories)
        out += "timestamp: %s\n" % int(time.mktime(post["dateCreated"].timetuple()))
        return unicode(out)

    key = make_post_key(post)
    d = make_dir(key)
    content = string.replace(post["description"], images_url, new_images_url)
    write_file(d + "content", content)
    write_file(d + "metadata", make_metadata())
    if comments:
        c = make_dir(d + "comments")
        for comment in comments:
            write_comment(comment, c)

def main(args):
    d_posts = tempfile.mkdtemp(prefix="wp2dir")
    d_images = tempfile.mkdtemp(prefix="wp2img")
    print "creating directories %s and %s" % (d_posts, d_images)

    _, url, images_url, new_images_url, user, passwd = args

    wp = WordPress(url, user, passwd)
    posts = wp.all_posts()
    for post in posts:
        comments = wp.post_comments(post)
        os.chdir(d_posts)
        write_post(post, post["categories"], comments, images_url, new_images_url)
        os.chdir(d_images)
        get_post_images(post, images_url, d_images)

if __name__ == "__main__":
    main(sys.argv)

