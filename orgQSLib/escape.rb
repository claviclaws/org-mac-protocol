#!/usr/bin/env ruby
require 'uri'
ARGV.each do|a|
puts URI.escape(%Q<#{a}>, /([^0-9A-Za-z-._~])/n)
end
