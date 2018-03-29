[![Build Status](https://travis-ci.org/wangbj/android-sparse-image.svg?branch=master)](https://travis-ci.org/wangbj/android-sparse-image)
# android-sparse-image

pack/unpack android sparse image files.

* simg2img

convert sparse image to (unsparsed) image

* img2simg

convert (unsparsed) image to sparse image

# Limitations

* no crc32 support
enable crc32 has big performance penalty, the android command line also doesn't provide options to enable crc
