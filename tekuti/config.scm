;; Tekuti
;; Copyright (C) 2008 Andy Wingo <wingo at pobox dot com>

;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 3 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; This is the main script that will launch tekuti.
;;
;;; Code:

(define-module (tekuti config)
  #:use-module (tekuti util)
  #:export (*host* *port* *backlog* *git-dir* *git* *public-url-base*
            *private-url-base* *debug* *admin-user* *admin-pass*
            *title* *name*))

(define *host* "127.0.0.1")
(define *port* 8081)
(define *backlog* 5)
(define *git-dir* "~/blog.git")
(define *git* "git")
(define *public-url-base* "/blog/")
(define *private-url-base* "/blog/")
(define *debug* #t)
(define *admin-user* "admin")
(define *admin-pass* "totingiini")
(define *title* "My blog")
(define *subtitle* "Just a blog, ok")
(define *name* "Joe Schmo")
