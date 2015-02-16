require 'bundler/setup'

require 'albacore'
require 'albacore/tasks/versionizer'
require 'albacore/ext/teamcity'
require 'albacore/tasks/release'

Configuration = ENV['Configuration'] || 'Release'

Albacore::Tasks::Versionizer.new :versioning

desc 'Perform fast build (warn: doesn\'t d/l deps)'
build :quick_build do |b|
  b.sln = 'src/Logary.sln'
end

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

  # attributes are required:
  a.attributes assembly_description: 'Logary is a high performance, multi-target logging, metric and health-check library for mono and .Net.',
               assembly_configuration: Configuration,
               assembly_company: 'Logibit AB',
               assembly_copyright: "(c) #{Time.now.year} by Henrik Feldt",
               assembly_version: ENV['LONG_VERSION'],
               assembly_file_version: ENV['LONG_VERSION'],
               assembly_informational_version: ENV['BUILD_VERSION']
  # optional, given an Albacore::Project and the configuration hash of the AsmVer task type
  # example:
  a.handle_config do |proj, conf|
    conf.namespace = conf.namespace + "AsmVer"
    conf
  end
end


build :clean_sln do |b|
  b.target = 'Clean'
  b.sln = 'src/Logary.sln'
  b.prop 'Configuration', Configuration
end

desc 'clean'
task :clean => [:clean_sln] do
  FileUtils.rm_rf 'build'
end

def maybe_sign conf
  pfx, pass, spc, pvk = [
    ENV['LOGARY_SIGN_ASSEMBLY_PFX'],
    ENV['LOGARY_SIGN_ASSEMBLY_PASSWORD'],
    ENV['LOGARY_SIGN_ASSEMBLY_SPC'],
    ENV['LOGARY_SIGN_ASSEMBLY_PVK']
  ]

  return unless ((pfx && pass) || (spc && pvk))

  info 'signing assembly'

  if pfx && pass
    info 'signing assembly with pfx'
    conf.prop 'SignAssemblyPfx', pfx
    conf.prop 'SignAssemblyPassword', pass
  else
    info 'signing assembly with spc'
    conf.prop 'SignAssemblySPC', spc
    conf.prop 'SignAssemblyPVK', pvk
  end
end

desc 'perform full build'
build :build => [:versioning, :assembly_info, :restore] do |b|
  b.prop 'Configuration', Configuration
  b.sln = 'src/Logary.sln'
  maybe_sign b
end

directory 'build/pkg'

nugets_pack :nugets_quick => :versioning do |p|
  p.configuration = Configuration
  p.files         = FileList['src/**/*.{csproj,fsproj,nuspec}'].
    exclude('src/Fsharp.Actor/*.nuspec').
    exclude(/Fracture|Example|Tests|Spec|Health|sample|packages/)
  p.out           = 'build/pkg'
  p.exe           = 'tools/NuGet.exe'
  p.with_metadata do |m|
    m.description = 'Logary is a high performance, multi-target logging, metric and health-check library for mono and .Net.'
    m.authors     = 'Henrik Feldt, Logibit AB'
    m.copyright   = 'Henrik Feldt, Logibit AB'
    m.version     = ENV['NUGET_VERSION']
    m.tags        = 'logging f# nosql log4net nlog serilog log logs metrics metrics.net measure performance perf semantic structured'
    m.license_url = 'https://www.apache.org/licenses/LICENSE-2.0.html'
    m.icon_url    = 'https://raw.githubusercontent.com/logary/logary-assets/master/graphics/LogaryLogoSquare32x32.png'
  end
end

desc 'package nugets - finds all projects and package them'
task :nugets => ['build/pkg', :versioning, :build, :nugets_quick]

task :tests_unit do
  Dir.glob("src/*.Tests/bin/#{Configuration}/*.Tests.exe").
    reject { |exe| exe.include? 'SQL' or exe.include? '.DB' or exe.include? 'Logentries' or exe.include? 'TOML' }.
    each do |exe|
    system exe, clr_command: true
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
             %W|push #{path}|,
             clr_command: true
    rescue => e
      error e
    end
  end
end

desc 'run unit tests'
task :tests => [:build, :tests_unit]

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
