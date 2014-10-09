require 'albacore'
require 'albacore/project'
require 'albacore/paths'

def install_nuget name
  unless Dir.exist? "tools/#{name}"
    system 'tools/NuGet.exe',
           %W|install -ExcludeVersion -OutputDirectory tools #{name}|,
           clr_command: true
  end
end

def pack files,
         tool = 'tools/ilmerge/ILMerge.exe',
         deps = %w(ilmerge)

  system tool,
    %W|/keyfile:src/signing/LogaryPublic.snk
       /ver:#{ENV['FORMAL_VERSION'] || '1.2.3'}
       /xmldocs /internalize
       /targetplatform:v4
       /out:build/Intelliplan.Logary-unit/Logary.dll
       src/Logary/bin/Release/Logary.dll|.
         concat(files),
         clr_command: true

  FileUtils.cp 'src/Logary/bin/Release/Logary.dll.config', 'build/Intelliplan.Logary-unit/Logary.dll.config'
end

directory 'build/Intelliplan.Logary-unit'

task :pack_logary_quick => 'build/Intelliplan.Logary-unit' do
  # ilpack_deps = %w(IKVM ILRepack)
  proj = Albacore::Project.new 'src/Logary/Intelliplan.Logary.fsproj'

  out = Paths.join('src/Logary', proj.output_path('Release')).to_s
  files =["src/Logary/bin/Release/FSharp.Actor.dll", "src/Logary/bin/Release/FSharp.Core.dll", "src/Logary/bin/Release/Intelliplan.JsonNet.NodaTime.dll", "src/Logary/bin/Release/Intelliplan.JsonNet.dll", "src/Logary/bin/Release/Newtonsoft.Json.dll", "src/Logary/bin/Release/NodaTime.dll", "src/Logary/bin/Release/policy.2.3.FSharp.Core.dll"]

  debug { "going to pack files: #{files.inspect}" }

  pack files
  pkg = Albacore::NugetModel::Package.new nil, files
end

desc 'pack the release of Logary with ilmerge'
task :pack_logary => [:compile]