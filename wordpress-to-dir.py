#!/usr/bin/env python

import sys
import tempfile
import MySQLdb as db
import os
import urllib
import time

cxn = None

def all_posts():
    cur = cxn.cursor()
    sql = ('select ID, post_author, post_date_gmt, post_content,'
           '       post_title, post_status, comment_status, post_name,'
           '       post_modified_gmt, post_content_filtered'
           '  from wp_posts WHERE post_status="publish" AND'
           '  post_type ="post" ORDER BY post_date DESC')
    cur.execute(sql)
    while True:
        row = cur.fetchone()
        if row:
            keys = ('id', 'author', 'date', 'content', 'title',
                    'status', 'comment_status', 'name', 'modified',
                    'content_filtered')
            yield dict(zip(keys, row))
        else:
            break

def post_categories(post):
    cur = cxn.cursor()
    sql = ('select name from wp_terms INNER JOIN wp_term_taxonomy ON'
            ' wp_terms.term_id = wp_term_taxonomy.term_id INNER JOIN'
            ' wp_term_relationships ON wp_term_relationships.term_taxonomy_id = '
            ' wp_term_taxonomy.term_taxonomy_id WHERE'
            ' wp_term_relationships.object_id=%s')
    cur.execute(sql, (post['id'],))
    return [row[0] for row in cur.fetchall()]

def post_comments(post):
    cur = cxn.cursor()
    sql = ('select comment_ID, comment_author, comment_author_email,'
           '       comment_author_url, comment_author_IP,'
           '       comment_date, comment_date_gmt, comment_content, comment_approved'
           '  from wp_comments where comment_post_ID=%s and comment_approved!=\'spam\'')
    cur.execute(sql, (post['id'],))
    keys = ('id', 'author', 'author_email', 'author_url', 'author_ip',
            'date', 'date-gmt', 'content', 'approved')
    return [dict(zip(keys, row)) for row in cur.fetchall()]

def write_file(path, content):
    f = open(path, 'w')
    f.write(content)
    f.close()

def make_dir(path):
    os.mkdir(path)
    return path + '/'

def write_comment(comment, dir):
    def make_metadata():
        out = ''
        for k, v in comment.items():
            if k not in ('content',):
                out += '%s: %s\n' % (k, v)
        date = comment['date-gmt'] or comment['date']
        out += 'timestamp: %s\n' % int(time.mktime(date.timetuple()))
        return out

    write_file(dir + str(comment['id']),
               make_metadata() + '\n' + comment['content'])

def make_post_key(post):
    d = post['date']
    pre = '%d/%02d/%02d/%s' % (d.year, d.month, d.day, post['name'])
    return urllib.quote(pre, '').lower()

def write_post(post, categories, comments):
    def make_metadata():
        out = ''
        for k, v in post.items():
            if k not in ('content', 'content_filtered'):
                out += '%s: %s\n' % (k, v)
        out += 'tags: %s\n' % ', '.join(categories)
        out += 'timestamp: %s\n' % int(time.mktime(post['date'].timetuple()))
        return out

    key = make_post_key(post)
    d = make_dir(key)
    write_file(d + 'content', post['content'])
    write_file(d + 'content-filtered', post['content_filtered'])
    write_file(d + 'metadata', make_metadata())
    if comments:
        c = make_dir(d + 'comments')
        for comment in comments:
            write_comment(comment, c)

def main(args):
    global cxn
    d = tempfile.mkdtemp(prefix='wp2dir')
    print 'writing dir', d
    os.chdir(d)
    _, host, user, passwd, database = args
    cxn = db.connect(host=host, user=user, passwd=passwd, db=database)
    cxn.cursor().execute("set names 'utf8'")
    for post in all_posts():
        write_post (post, post_categories (post), post_comments (post))
    
if __name__ == '__main__':
    main(sys.argv)
