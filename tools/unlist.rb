#!/usr/bin/env ruby
require 'rake'
require 'json'

package = ARGV[0] || raise('Must supply name of package')
api_key = ENV['NUGET_KEY'] || raise('Must have NUGET_KEY')

package_info = JSON.parse(`curl --silent -L "https://api.nuget.org/v3-flatcontainer/#{package}/index.json"`)

$stdout.puts "Package versions:"
$stdout.puts package_info.inspect

package_info['versions'].
    keep_if do |version|
  if ARGV[1] then
    "v#{version}".include? ARGV[1]
  else
    true
  end
end.map do |version|
  $stdout.write "Delete package #{package} v#{version} [y/N]? "
  [version, $stdin.gets.chomp == 'y']
end.each do |version, answer|
  if answer then
    $stdout.puts "Deleting #{package} v#{version}"
    sh "yes | mono --debug nuget.exe delete #{package} #{version} #{api_key} -source https://api.nuget.org/v3/index.json"
  end
end

