require 'bundler/setup'
require 'pathname'
require 'albacore'
require 'albacore/tasks/versionizer'
require 'albacore/tasks/release'

Configuration = ENV['CONFIGURATION'] || 'Release'
LogaryStrongName = (ENV['LOGARY_STRONG_NAME'] && true) || false

Albacore::Tasks::Versionizer.new :versioning

task :paket_bootstrap do
  system 'tools/paket.bootstrapper.exe', clr_command: true unless \
         File.exists? 'tools/paket.exe'
end

desc 'restore all nugets as per the packages.config files'
task :restore => :paket_bootstrap do
  system 'tools/paket.exe', 'restore', clr_command: true
end

desc 'create assembly infos'
asmver_files :assembly_info => :versioning do |a|
  a.files = FileList['**/*proj'] # optional, will find all projects recursively by default
  a.attributes assembly_description: 'Logary is a high performance, multi-target logging, metric and health-check library for mono and .Net.',
               assembly_configuration: Configuration,
               assembly_company: 'Logibit AB',
               assembly_copyright: "(c) #{Time.now.year} by Henrik Feldt",
               assembly_version: ENV['LONG_VERSION'],
               assembly_file_version: ENV['LONG_VERSION'],
               assembly_informational_version: ENV['BUILD_VERSION']

  a.handle_config do |proj, conf|
    conf.namespace = conf.namespace + "AsmVer"
    if LogaryStrongName
      path = Pathname.new(File.join(FileUtils.pwd, 'tools/logary.snk')).
          relative_path_from(Pathname.new(File.join(FileUtils.pwd, 'src', proj.proj_path_base, '..'))).
          to_s
      conf.change_attributes do |attrs|
        attrs[:assembly_key_file] = path unless proj.proj_filename.include? 'csproj' or proj.proj_filename.include? 'Services' or proj.proj_filename.include? 'Tests'
      end
    end
    conf
  end
end

task :paket_replace do
  sh %{ruby -pi.bak -e "gsub(/module Aether/, 'module Logary.Utils.Aether')" paket-files/xyncro/aether/src/Aether/Aether.fs}
  sh %{ruby -pi.bak -e "gsub(/module Chiron/, 'module Logary.Utils.Chiron')" paket-files/xyncro/chiron/src/Chiron/Chiron.fs}
  sh %{ruby -pi.bak -e "gsub(/module.* YoLo/, 'module internal Logary.YoLo')" paket-files/haf/YoLo/YoLo.fs}
  sh %{ruby -pi.bak -e "gsub(/module Hopac/, 'namespace Logary.Internals')" paket-files/logary/RingBuffer/RingBuffer.fs}
  sh %{ruby -pi.bak -e "gsub(/namespace Logary.Facade/, 'namespace Libryy.Logging')" paket-files/logary/logary/src/Logary.Facade/Facade.fs}
end

build :clean_sln do |b|
  b.target = 'Clean'
  b.sln = 'src/v4.sln'
  b.prop 'Configuration', Configuration
end

desc 'clean'
task :clean => [:clean_sln] do
  FileUtils.rm_rf 'build'
end

def maybe_sign conf
  pfx, pass, sign = [
    ENV['LOGARY_SIGN_ASSEMBLY_PFX'],
    ENV['LOGARY_SIGN_ASSEMBLY_PASSWORD'],
    ENV['LOGARY_SIGN_ASSEMBLY'] || File.exists?('./tools/logary.password')
  ]

  if LogaryStrongName
    conf.prop 'StrongNameAssembly', 'true'
  end

  return unless sign
  pass ||= File.read 'tools/logary.password'

  info 'signing assembly'

  if pfx && pass
    info 'signing assembly with pfx'
    conf.prop 'SignAssemblyPfx', pfx
    conf.prop 'SignAssemblyPassword', pass
  else
    # brew install osslsigncode
    info 'signing assembly with spc/pvk'
    conf.prop 'SignAssembly', 'true'
    conf.prop 'SignAssemblyPassword', pass
  end
end

desc 'Perform quick build'
build :build_quick do |b|
  b.prop 'Configuration', Configuration
  b.sln = 'src/Logary.sln'
  maybe_sign b
end

desc 'Perform full build'
task :build => [:versioning, :assembly_info, :restore, :paket_replace, :build_quick]

directory 'build/pkg'

nugets_pack :nugets_quick => :versioning do |p|
  p.configuration = Configuration
  p.files         = FileList['src/**/*.{csproj,fsproj,nuspec}'].
    exclude(/Example|Tests|Spec|Health|Logary[.]Facade|sample|packages/)
  p.out           = 'build/pkg'
  p.exe           = 'tools/NuGet.exe'
  p.with_metadata do |m|
    m.description = 'Logary is a high performance, multi-target logging, metric and health-check library for mono and .Net.'
    m.authors     = 'Henrik Feldt, Logibit AB'
    m.copyright   = 'Henrik Feldt, Logibit AB'
    m.version     = ENV['NUGET_VERSION']
    m.tags        = 'logging f# log logs metrics metrics.net measure gauge semantic structured'
    m.license_url = 'https://www.apache.org/licenses/LICENSE-2.0.html'
    m.icon_url    = 'https://raw.githubusercontent.com/logary/logary-assets/master/graphics/LogaryLogoSquare32x32.png'
  end
end

desc 'package nugets - finds all projects and package them'
task :nugets => ['build/pkg', :versioning, :build, :nugets_quick]

task :tests_unit do
  Dir.glob("src/tests/**/bin/#{Configuration}/*.Tests.exe").
    reject { |exe| exe.include? '.DB' }.
    keep_if { |exe| !exe.include?('Mailgun') || (ENV['MAILGUN_API_KEY'] && exe.include?('Mailgun')) }.
    keep_if { |exe| !exe.include?('ElmahIO') || (ENV['ELMAH_IO_LOG_ID'] && exe.include?('ElmahIO')) }.
    each do |exe|
    system exe, %w|--sequenced|, clr_command: true
  end
end

desc 'run integration tests'
task :tests_integration do
  system "src/tests/Logary.IntegrationTests/bin/#{Configuration}/Logary.IntegrationTests.exe", clr_command: true
end

Albacore::Tasks::Release.new :release,
                             pkg_dir: 'build/pkg',
                             depend_on: :nugets,
                             nuget_exe: 'tools/NuGet.exe',
                             api_key: ENV['NUGET_KEY']
desc 'push all packages in build/pkg'
task :nugets_push do
  Dir.glob 'build/pkg/*.nupkg' do |path|
    begin
      system 'tools/NuGet.exe',
             %W|push #{path} #{ENV['NUGET_KEY']} -source https://api.nuget.org/v3/index.json|,
             clr_command: true
    rescue => e
      error e
    end
  end
end

test_runner :tests_spec do |tests|
  tests.files = FileList['src/tests/Logary.CSharp.Tests/bin/Release/*.Tests.dll']
  tests.exe = 'packages\Machine.Specifications.Runner.Console\tools\mspec-clr4.exe'
  #tests.add_parameter ''
  #tests.native_exe # when you don't want to use 'mono' as the native executable on non-windows systems
end

desc 'run unit tests'
task :tests => [:build, :tests_unit, :tests_spec]

task :default => [:tests, :nugets]

namespace :docs do
  task :pre_reqs do
    { 'FAKE'              => '2.17.9',
      'FSharp.Formatting' => '2.4.10' }.
      each do |name, version|
      system 'tools/NuGet.exe', %W|
        install #{name} -OutputDirectory tools -Version #{version} -ExcludeVersion
      |, clr_command: true
    end
  end

  directory 'build/api'
  directory 'docs/files'
  directory 'docs/content'

  desc 'build docs'
  task :build => [:pre_reqs, 'docs/files', 'docs/content', 'build/api'] do
    system 'tools/FAKE/tools/Fake.exe', 'tools/docs.fsx', clr_command: true
  end

  # unused!! for reference only
  task :build2 => :pre_reqs do
    system 'tools/FSharp.Formatting.CommandTool/tools/fsformatting.exe', %w|
     metadataFormat --generate
                    --sourceRepo https://github.com/logary/logary
                    --sourceFolder /src/Logary
                    --dllFiles src/Logary/bin/Release/Logary.dll
                    --outDir docs/output-api
                    --layoutRoots docs/layouts
                    --libDirs src/Logary/bin/Release
    |, clr_command: true
  end
end
