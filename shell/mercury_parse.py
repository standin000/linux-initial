#!/usr/bin/env python
# sudo apt install libxml2-dev
# sudo apt install libxslt-dev
# sudo apt install python2-dev
# sudo apt install python-pip
# pip install requests --user
# pip install wheel-0.31.1-py2.py3-none-any.whl  --user
# pip install bs4 --user
# pip install lxml --user 256M memory is not enough
# pip install html5lib-1.0.1-py2.py3-none-any.whl --user # use html5lib instead 
# http://m.7624.net/iyule/5147451/20180516A1H8K600.html
# https://news.boxun.com/news/gb/china/2018/05/201805071304.shtml is ng

import os, sys, requests, base64, urllib, codecs, urlparse
from bs4 import BeautifulSoup
from optparse import OptionParser

_apiKey = 'nGc0ya2J7z2aalFrGa8Gx3Q1o8grGFsn3cz58EJy'

def get_content_to_file( url, htmlfilename ):
    with requests.Session( ) as s:
        s.headers =  { 'Content-Type' : 'application/json',
                       'x-api-key' : _apiKey }
        response = s.get( 'https://mercury.postlight.com/parser',
                          params = { 'url' : url })
        if response.status_code != 200:
            return 'Error, no data from %s' % url
        data = response.json( )
        content = data['content']
        title = data['title']
        date_publish_string = data['date_published']
        excerpt = data['excerpt']
        url = data['url']
        html = BeautifulSoup( content, 'lxml' )
       # html = BeautifulSoup( content, 'html5lib' )
        #
        ## now all png objects to inline
        if not os.path.exists(title):
            os.mkdir(title)
            
        for img in html.find_all('img'):
            imgURL = img['src']
            split = urlparse.urlsplit(imgURL)
            filename = "./%s/"%(title) + split.path.split("/")[-1]
            urllib.urlretrieve(imgURL, filename)
            img['src'] = filename
            # if imgURL.lower( ).endswith('.png'):                
            #     img_64 = "data:image/png;base64," + base64.b64encode( urllib.urlopen( imgURL ).read( ) )
            # elif imgURL.lower( ).endswith( '.jpg' ):
            #     img_64 = "data:image/jpg;base64," + base64.b64encode( urllib.urlopen( imgURL ).read( ) )
            # else:
            #     img_64 = None
            # #
            # if img_64 is not None:
            #     img['src'] = img_64
        if not os.listdir(title) :
            os.rmdir(title)

        htag = html.new_tag( 'head' )
        mtag = html.new_tag( 'meta' )
        mtag['charset'] = 'utf-8'
        htag.append( mtag )
        html.insert(0, htag )
        if not htmlfilename :
            htmlfilename = title + '.html'
            
        with codecs.open( htmlfilename, 'w', 'utf-8') as openfile:
            openfile.write('%s\n' % html.prettify( ) )

if __name__=='__main__':
    parser = OptionParser( )
    parser.add_option('-i', '--url', dest='url', type=str, action='store',
                      help = "Name of the URL to output into an HTML file.")
    parser.add_option('-o', '--htmlfile', dest='htmlfile', type=str, action='store',
                      help = 'Name of the HTML file to store the underlying data.')
    opts, args = parser.parse_args( )
#    assert(all(map(lambda tok: tok is not None, ( opts.url, opts.htmlfile ) ) ) )
#    assert( os.path.basename( opts.htmlfile ).endswith( '.html' ) )
    get_content_to_file( opts.url, opts.htmlfile )
