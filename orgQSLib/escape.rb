#!/usr/bin/env ruby

# escape.rb --- script to provide URI escaping missing in AppleScript

# Copyright (C) 2009, 2010 Christopher Suckling

# Author: Christopher Suckling <suckling at gmail dot com>

# This file is Free Software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# It is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
# License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

# Vesion: 0.634

# Commentary

# Part of org-mac-protocol

# Installation

# 1) Copy to ~/Library/Scripts/orgQSLib/

# Please see org-mac-protocol.org for full installation and usage
# instructions

require 'uri'
ARGV.each do|a|
puts URI.escape(%Q<#{a}>, /([^0-9A-Za-z-._~])/n)
end
