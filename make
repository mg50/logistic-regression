#!/usr/bin/env ruby

require 'pry'

class File
  def replace!(text, replacement)
    content = read
    content.sub!(text, replacement)
    seek 0
    write(content)
    seek 0
  end
end

ELM_MODULE_NAME = 'Main'
puts `elm make ./elm/#{ELM_MODULE_NAME}.elm --yes --output build/#{ELM_MODULE_NAME}.js`

# native_js = File.read('./elm/Native/Numeric.js')

# js_path = "./build/#{ELM_MODULE_NAME}.js"

# File.open(js_path, 'r+') do |f|
#   f.replace!('Elm.Native.Basics =', "#{native_js}\nElm.Native.Basics =")
#   f.replace!('var eigen = $',
#              "var $Native$Numeric = Elm.Native.Numeric.make(_elm);\nvar eigen = $")
# end
