#!/usr/bin/env python

import sys
import tempfile
import MySQLdb as db
import os

cxn = None

def all_posts():
  `ID` bigint(20) unsigned NOT NULL auto_increment,
  `post_author` bigint(20) NOT NULL default '0',
  `post_date` datetime NOT NULL default '0000-00-00 00:00:00',
  `post_date_gmt` datetime NOT NULL default '0000-00-00 00:00:00',
  `post_content` longtext NOT NULL,
  `post_title` text NOT NULL,
  `post_category` int(4) NOT NULL default '0',
  `post_excerpt` text NOT NULL,
  `post_lat` float default NULL,
  `post_lon` float default NULL,
  `post_status` enum('publish','draft','private','static','object','attachment') NOT NULL default 'publish',
  `comment_status` enum('open','closed','registered_only') NOT NULL default 'open',
  `ping_status` enum('open','closed') NOT NULL default 'open',
  `post_password` varchar(7) NOT NULL default '',
  `post_name` varchar(67) NOT NULL default '',
  `to_ping` text NOT NULL,
  `pinged` text NOT NULL,
  `post_modified` datetime NOT NULL default '0000-00-00 00:00:00',
  `post_modified_gmt` datetime NOT NULL default '0000-00-00 00:00:00',
  `post_content_filtered` text NOT NULL,
  `post_parent` bigint(20) NOT NULL default '0',
  `guid` varchar(85) NOT NULL default '',
  `menu_order` int(11) NOT NULL default '0',
  `post_type` varchar(34) NOT NULL default '',
  `post_mime_type` varchar(34) NOT NULL default '',
  `comment_count` bigint(20) NOT NULL default '0',
  PRIMARY KEY  (`ID`),
  KEY `post_date` (`post_date`),
  KEY `post_date_gmt` (`post_date_gmt`),
  KEY `post_name` (`post_name`),
  KEY `post_status` (`post_status`)
    cur = cxn.cursor()
    sql = ('select ID, post_author, post_date_gmt, post_content,'
           '       post_title, post_status, comment_status, post_name,'
           '       post_modified_gmt, post_content_filtered'
           '  from wp_posts')
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

def write_post(post):
    print post['name']

def main(args):
    global cxn
    d = tempfile.mkdtemp(prefix='wp2dir')
    print 'writing dir', d
    os.chdir(d)
    _, host, user, passwd, db = args
    cxn = db.connect(host=host, user=user, passwd=passwd, db=db)
    for post in all_posts():
        write_post (post, post_categories (post), post_comments (post))
    
if __name__ == '__main__':
    main(sys.argv)
