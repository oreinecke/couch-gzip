Couch-gzip
==========

This plugin wraps gzip encoding around selected CouchDB reponses. As most of
them are JSON and document ids share a common prefix, transferred data is
greatly reduced.

Installation
------------

Compile the single erl script and place it in a meaningful sub-folder of the
lib folder of your CouchDB installation, and then append the content of
[couch-gzip.ini](priv/default.d/couch-gzip.ini) to your local.ini. While these
sound like horrible instructions, they are probably the easiest way to install
CouchDB plugins on Windows.

On Linux you can do `make plugin`, upload the plugin slug to your CouchDB, and
post the appropriate plugin-data to `/_plugins`. This way it gets listed as a
regular plugin and can be uninstalled just as easy.

How To Use It
-------------

Just prefix the URL with `_gzip/`, for instance

```
$ curl --verbose --compressed \
  localhost:5984/_gzip/db/_all_docs?descending=true > /dev/null
* 
* yada yada yada
* 
> GET /_gzip/db/_all_docs?descending=true HTTP/1.1
> User-Agent: curl/7.32.0
> Host: localhost:5984
> Accept: */*
> Accept-Encoding: deflate, gzip           <- curl accepts gzip encoding ...
>
* yada more uninteresting info
< 
< etag: "6PSMRU3WOV3GHU8OI55Y786M4"
< date: Tue, 24 Mar 2015 16:30:09 GMT
< content-type: text/plain; charset=utf-8
< Content-Length: 317660
< Content-encoding: gzip                   <- ... and in return receives gzip.
< cache-control: must-revalidate
<
{ [data not shown]
* STATE: PERFORM => DONE handle 0x60002d0b0; line 1581 (connection #0)
100  310k  100  310k    0     0   821k      0 --:--:-- --:--:-- --:--:--  831k
* Connection #0 to host localhost left intact
```

Compression is applied only if
 - method is `GET` or `POST`,
 - return code is `200 OK`, and
 - content type is listed in `/_config/attachments/compressible_types`.

Known Limitations
-----------------

- No SSL,
- will most likely crash for large response bodies; in this case, it should
  default to an uncompressed response,
- stacks a couple of 100ms on top of the response time.
