# -*- coding: utf-8 -*-

"""
Copyright (c) 2009 Barry Schwartz

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

import fontforge

#--------------------------------------------------------------------------

def copyright_notice(year, copyright_holder):
    return 'Copyright (c) ' + str(year) + ' ' + copyright_holder

def ofl_url():
    return 'http://scripts.sil.org/OFL'

def ofl_notice(year, copyright_holder, reserved_name):
    return ('Copyright (c) ' + str(year) + ', ' + copyright_holder +
            ',\nwith Reserved Font Name ' + reserved_name + '.\n' +
            '''

This Font Software is licensed under the SIL Open Font License, Version 1.1.
This license is available with a FAQ at:
http://scripts.sil.org/OFL
''')

def mit_url():
    return 'http://www.opensource.org/licenses/mit-license.php'

def mit_notice(year, copyright_holder, reserved_name = None):
    return ('Copyright (c) ' + str(year) + ' ' + copyright_holder + '\n' + '''

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
''')

#--------------------------------------------------------------------------

def apply_license(font, license_notice, license_url, copyright_year, copyright_holder, name_prefix = None):
    if name_prefix:
        font.fontname = name_prefix + font.fontname
        font.familyname = name_prefix + ' ' + font.familyname
        font.fullname = name_prefix + ' ' + font.fullname
        sfnt_names = font.sfnt_names
        for (lang, name_id, name) in sfnt_names:
            if (name_id in ['Family', 'Preferred Family', 'WWS Family'] and
                name[:len(name_prefix) + 1] != name_prefix + ' '):
                font.appendSFNTName(lang, name_id, name_prefix + ' ' + name)
    font.copyright = copyright_notice(copyright_year, copyright_holder)
    font.appendSFNTName('English (US)', 'Copyright', font.copyright)
    font.appendSFNTName('English (US)', 'License', license_notice(copyright_year, copyright_holder,
                                                                  reserved_name = font.fullname))
    font.appendSFNTName('English (US)', 'License URL', license_url())

#--------------------------------------------------------------------------
