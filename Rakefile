
# Encoding: utf-8
require 'bundler/setup'
require 'albacore'
require 'albacore/nuget_model'
require 'albacore/project'
require 'albacore/tools'
require 'albacore/tasks/versionizer'
require 'albacore/tasks/release'
require 'albacore/task_types/nugets_pack'
require 'albacore/task_types/asmver'
require './tools/paket_pack'
require 'semver'

Albacore::Tasks::Versionizer.new :versioning

include ::Albacore::NugetsPack

suave_description = 'Suave is a simple web development F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition.'

Configuration = ENV['CONFIGURATION'] || 'Release'
Platform = ENV['MSBUILD_PLATFORM'] || 'Any CPU'

task :paket_files do
  sh %{ruby -pi.bak -e "gsub(/module Aether/, 'module Logary.Utils.Aether')" paket-files/xyncro/aether/src/Aether/Aether.fs}
  sh %{ruby -pi.bak -e "gsub(/module Chiron/, 'module Logary.Utils.Chiron')" paket-files/xyncro/chiron/src/Chiron/Chiron.fs}
  sh %{ruby -pi.bak -e "gsub(/module.* YoLo/, 'module internal Logary.YoLo')" paket-files/haf/YoLo/YoLo.fs}
  sh %{ruby -pi.bak -e "gsub(/module Hopac/, 'namespace Logary.Internals')" paket-files/logary/RingBuffer/RingBuffer.fs}
  sh %{ruby -pi.bak -e "gsub(/namespace Logary.Facade/, 'namespace Libryy.Logging')" paket-files/logary/logary/src/Logary.Facade/Facade.fs}
  sh %{ruby -pi.bak -e "gsub(/namespace Logary.Facade/, 'namespace Cibryy.Logging')" paket-files/logary/logary/src/Logary.CSharp.Facade/Facade.cs}
end

desc "Restore paket.exe"
task :restore_paket do
  system 'tools/paket.bootstrapper.exe', clr_command: true unless
    File.exists? 'tools/paket.exe'
end

task :paket_restore do
  system 'tools/paket.exe', 'restore', clr_command: true
  system 'tools/paket.exe', %w|restore group Examples|, clr_command: true
  system 'tools/paket.exe', %w|restore group netcore|, clr_command: true
end

desc 'Restore all packages'
task :restore => [:restore_paket, :paket_restore, :paket_files]

dotnet_exe_path = "dotnet"

desc 'Clean the CoreCLR dirs'
task :dotnetclean => :restore do
  system dotnet_exe_path, %W|clean src/Logary.NetCore.sln -v n|
end

desc 'Restore the CoreCLR binaries'
task :dotnetrestore => :dotnetclean do
  system dotnet_exe_path, %W|restore src/Logary.NetCore.sln -v n|
end

desc 'Builds the CoreCLR binaries'
task :dotnetbuild => :dotnetrestore do
  system dotnet_exe_path, %W|build src/Logary.NetCore.sln -v n|
end

directory 'build/pkg'
package_dist_path = File.expand_path "build/pkg"

desc 'Packs the binaries'
task :dotnetpack => [:dotnetbuild, :versioning, 'build/pkg'] do
  projects = FileList['src/**/*.{csproj,fsproj}'].exclude(/Example|Services|Tests|Spec|Rutta|Health|Logary[.]Facade|Logary[.]CSharp[.]Facade|sample|packages/i)
  projects.each do |proj|
    system dotnet_exe_path, %W|pack #{proj} -o #{package_dist_path} /p:PackageVersion=#{ENV['NUGET_VERSION']} -v n|
  end
end

task :default => [:dotnetpack]
# desc 'create assembly infos'
# asmver_files :assembly_info => :versioning do |a|
#   a.files = FileList['**/*proj'] # optional, will find all projects recursively by default
#   a.attributes assembly_description: 'Logary is a high performance, multi-target logging, metric and health-check library for mono and .Net.',
#                assembly_configuration: Configuration,
#                assembly_company: 'Logibit AB',
#                assembly_copyright: "(c) #{Time.now.year} by Henrik Feldt",
#                assembly_version: ENV['LONG_VERSION'],
#                assembly_file_version: ENV['LONG_VERSION'],
#                assembly_informational_version: ENV['BUILD_VERSION']

#   a.handle_config do |proj, conf|
#     conf.namespace = conf.namespace + "AsmVer"
#     if LogaryStrongName
#       path = Pathname.new(File.join(FileUtils.pwd, 'tools/logary.snk')).
#           relative_path_from(Pathname.new(File.join(FileUtils.pwd, 'src', proj.proj_path_base, '..'))).
#           to_s
#       conf.change_attributes do |attrs|
#         attrs[:assembly_key_file] = path unless proj.proj_filename.include? 'csproj' or proj.proj_filename.include? 'Services' or proj.proj_filename.include? 'Tests'
#       end
#     end
#     conf
#   end
# end

# task :paket_replace do
#   sh %{ruby -pi.bak -e "gsub(/module Aether/, 'module Logary.Utils.Aether')" paket-files/xyncro/aether/src/Aether/Aether.fs}
#   sh %{ruby -pi.bak -e "gsub(/module Chiron/, 'module Logary.Utils.Chiron')" paket-files/xyncro/chiron/src/Chiron/Chiron.fs}
#   sh %{ruby -pi.bak -e "gsub(/module.* YoLo/, 'module internal Logary.YoLo')" paket-files/haf/YoLo/YoLo.fs}
#   sh %{ruby -pi.bak -e "gsub(/module Hopac/, 'namespace Logary.Internals')" paket-files/logary/RingBuffer/RingBuffer.fs}
#   sh %{ruby -pi.bak -e "gsub(/namespace Logary.Facade/, 'namespace Libryy.Logging')" paket-files/logary/logary/src/Logary.Facade/Facade.fs}
#   sh %{ruby -pi.bak -e "gsub(/namespace Logary.Facade/, 'namespace Cibryy.Logging')" paket-files/logary/logary/src/Logary.CSharp.Facade/Facade.cs}
# end

# build :clean_sln do |b|
#   b.target = 'Clean'
#   b.sln = 'src/v4.sln'
#   b.prop 'Configuration', Configuration
# end

# desc 'clean'
# task :clean => [:clean_sln] do
#   FileUtils.rm_rf 'build'
# end

# def maybe_sign conf
#   pfx, pass, sign = [
#     ENV['LOGARY_SIGN_ASSEMBLY_PFX'],
#     ENV['LOGARY_SIGN_ASSEMBLY_PASSWORD'],
#     ENV['LOGARY_SIGN_ASSEMBLY'] || File.exists?('./tools/logary.password')
#   ]

#   if LogaryStrongName
#     conf.prop 'StrongNameAssembly', 'true'
#   end

#   return unless sign
#   pass ||= File.read 'tools/logary.password'

#   info 'signing assembly'

#   if pfx && pass
#     info 'signing assembly with pfx'
#     conf.prop 'SignAssemblyPfx', pfx
#     conf.prop 'SignAssemblyPassword', pass
#   else
#     # brew install osslsigncode
#     info 'signing assembly with spc/pvk'
#     conf.prop 'SignAssembly', 'true'
#     conf.prop 'SignAssemblyPassword', pass
#   end
# end

# desc 'Perform quick build'
# build :build_quick do |b|
#   b.prop 'Configuration', Configuration
#   b.sln = 'src/Logary.sln'
#   maybe_sign b
# end

# desc 'Perform full build'
# task :build => [:versioning, :assembly_info, :restore, :paket_replace, :build_quick]

# directory 'build/pkg'

# task :nugets_quick => [:versioning, 'build/pkg'] do
#   projects = FileList['src/**/*.{csproj,fsproj}'].exclude(/Example|Tests|Spec|Rutta|Health|Logary[.]Facade|Logary[.]CSharp[.]Facade|sample|packages/i)
#   knowns = Set.new(projects.map { |f| Albacore::Project.new f }.map { |p| p.id })
#   authors = "Henrik Feldt, Logibit AB"
#   projects.each do |f|
#     p = Albacore::Project.new f
#     n = create_nuspec p, knowns
#     d = get_dependencies n
#     m = %{type file
# id #{p.id}
# version #{ENV['NUGET_VERSION']}
# title #{p.id}
# authors #{authors}
# owners #{authors}
# tags logging f# log logs metrics metrics.net measure gauge semantic structured
# description Logary is a high performance, multi-target logging, metric and health-check library for mono and .Net.
# language en-GB
# copyright #{authors}
# licenseUrl https://www.apache.org/licenses/LICENSE-2.0.html
# projectUrl https://github.com/logary/logary
# iconUrl https://raw.githubusercontent.com/logary/logary-assets/master/graphics/LogaryLogoSquare32x32.png
# files
#   #{p.proj_path_base}/#{p.output_dll Configuration} ==\> lib/net452
#   #{p.proj_path_base}/#{p.output_dll(Configuration).to_s.sub('.dll', '.xml')} ==\> lib/net452
# releaseNotes
#   #{n.metadata.release_notes.each_line.reject{|x| x.strip == ""}.join}
# dependencies
#   #{d}
# }
#     begin
#       File.open("paket.template", "w") do |template|
#         template.write m
#       end
#       system "tools/paket.exe", %w|pack output build/pkg|, clr_command: true
#     ensure
#       File.delete "paket.template"
#     end
#   end
# end

# desc 'package nugets - finds all projects and package them'
# task :nugets => ['build/pkg', :versioning, :build, :nugets_quick]

# task :tests_unit do
#   Dir.glob("src/tests/**/bin/#{Configuration}/*.Tests.exe").
#     reject { |exe| exe.include? '.DB' }.
#     keep_if { |exe| !exe.include?('Mailgun') || (ENV['MAILGUN_API_KEY'] && exe.include?('Mailgun')) }.
#     keep_if { |exe| !exe.include?('ElmahIO') || (ENV['ELMAH_IO_LOG_ID'] && exe.include?('ElmahIO')) }.
#     keep_if { |exe| !exe.include?('Stackdriver') || (ENV["STACKDRIVER_PROJECT"] && ENV["STACKDRIVER_LOG"] && exe.include?('Stackdriver')) }.
#     each do |exe|
#     system exe, %W|--sequenced #{ENV['DEBUG'] ? "--debug" : ""}|, clr_command: true
#   end
# end

# desc 'run integration tests'
# task :tests_integration do
#   system "src/tests/Logary.IntegrationTests/bin/#{Configuration}/Logary.IntegrationTests.exe", clr_command: true
# end

# Albacore::Tasks::Release.new :release,
#                              pkg_dir: 'build/pkg',
#                              depend_on: :nugets,
#                              nuget_exe: 'tools/NuGet.exe',
#                              api_key: ENV['NUGET_KEY']
# desc 'push all packages in build/pkg'
# task :nugets_push do
#   Dir.glob 'build/pkg/*.nupkg' do |path|
#     begin
#       system 'tools/NuGet.exe',
#              %W|push #{path} #{ENV['NUGET_KEY']} -source https://api.nuget.org/v3/index.json|,
#              clr_command: true
#     rescue => e
#       error e
#     end
#   end
# end

# test_runner :tests_spec do |tests|
#   tests.files = FileList['src/tests/Logary.CSharp.Tests/bin/Release/*.Tests.dll']
#   tests.exe = 'packages\Machine.Specifications.Runner.Console\tools\mspec-clr4.exe'
#   #tests.add_parameter ''
#   #tests.native_exe # when you don't want to use 'mono' as the native executable on non-windows systems
# end

# desc 'run unit tests'
# task :tests => [:build, :tests_unit, :tests_spec]

# task :default => [:tests, :nugets]

# namespace :docs do
#   task :pre_reqs do
#     { 'FAKE'              => '2.17.9',
#       'FSharp.Formatting' => '2.4.10' }.
#       each do |name, version|
#       system 'tools/NuGet.exe', %W|
#         install #{name} -OutputDirectory tools -Version #{version} -ExcludeVersion
#       |, clr_command: true
#     end
#   end

#   directory 'build/api'
#   directory 'docs/files'
#   directory 'docs/content'

#   desc 'build docs'
#   task :build => [:pre_reqs, 'docs/files', 'docs/content', 'build/api'] do
#     system 'tools/FAKE/tools/Fake.exe', 'tools/docs.fsx', clr_command: true
#   end

#   # unused!! for reference only
#   task :build2 => :pre_reqs do
#     system 'tools/FSharp.Formatting.CommandTool/tools/fsformatting.exe', %w|
#      metadataFormat --generate
#                     --sourceRepo https://github.com/logary/logary
#                     --sourceFolder /src/Logary
#                     --dllFiles src/Logary/bin/Release/Logary.dll
#                     --outDir docs/output-api
#                     --layoutRoots docs/layouts
#                     --libDirs src/Logary/bin/Release
#     |, clr_command: true
#   end
# end
