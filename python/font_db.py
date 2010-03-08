# -*- coding: utf-8 -*-

"""
Copyright (c) 2010 Barry Schwartz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
"""

#--------------------------------------------------------------------------

import os
import shelve

#--------------------------------------------------------------------------

def create_temporary(font):
    if font.temporary is None:
        font.temporary = {}

def db_file_name(font):
    return font.fontname + '.db'

def db_open(font, flag = 'c'):
    if font.temporary is None or 'db' not in font.temporary or font.temporary['db'] is None:
        db_name = db_file_name(font)
        if flag not in ('c', 'n') and not os.path.exists(db_name):
            db = shelve.open(db_name, flag = 'c')
            db.close()
        db = shelve.open(db_name, flag = flag)
        create_temporary(font)
        font.temporary['db'] = db
    return font.temporary['db']

def db_create(font):
    return db_open(font, flag = 'c')

def db_close(font):
    if font.temporary is not None and 'db' in font.temporary and font.temporary['db'] is not None:
        font.temporary['db'].close()
        font.temporary['db'] = None

def db_exists(font):
    return os.path.exists(db_file_name(font))

def db_remove(font):
    if db_exists(font):
        db_close(font)
        os.remove(db_file_name(font))

def db_init_binding(font, key, default_value):
    db = db_open(font)
    if key not in db:
        db[key] = default_value

#--------------------------------------------------------------------------
