require 'albacore'
require 'albacore/project'
require 'albacore/paths'

# reference: 
# https://github.com/EventStore/EventStore/blob/dev/scripts/package-mono/package-mono.sh

def mkbundle bin, configuration = 'Release'
  fail 'on Windows, use another tool, e.g. ' if Albacore.windows?
  fail 'invalid "bin" parameter' unless bin
  material = Dir[Paths.join(bin, '*.{dll,config}')]
  mono_config = 'TODO'
  machine_config = 'TODO'
  # this doesn't help me much since I'm not merging into an executable
  system 'mkbundle',
       %w|-o -oo logary.a|.concat(material).concat(
         %W|--static --deps --config #{monoconfig} --machine-config #{machineconfig}|
       )
  system 'cc',
       %W|-o logary #{ES_COMPILE_FLAGS} testclient.c testclient.a $MONOPREFIX/lib/libmonosgen-2.0.a $MONOPREFIX/lib/libMonoPosixHelper.a|
end

def install_nuget name
  unless Dir.exist? "tools/#{name}"
    system 'tools/NuGet.exe',
           %W|install -ExcludeVersion -OutputDirectory tools #{name}|,
           clr_command: true
  end
end

def pack tool = 'tools/ILRepack/tools/ILRepack.exe'
         dir = 'src/Logary/bin/Release'
  files = %w|FSharp.Actor FSharp.Core Intelliplan.JsonNet.NodaTime
             Intelliplan.JsonNet Newtonsoft.Json NodaTime
             policy.2.3.FSharp.Core|.
          map { |f| "#{dir}/#{f}.dll" }

  system tool,
         %W|--keyfile:src/signing/LogaryPublic.snk --ver:#{ENV['FORMAL_VERSION']}
            --xmldocs --internalize --parallel --index --verbose
            --targetplatform:v4
            --out:Logary.dll
            --lib:tools/IKVM/lib|.
         concat(files),
         clr_command: true
end

desc 'pack the release of Logary with ILRepack'
task :pack do
  install_nuget 'IKVM'
  install_nuget 'ILRepack'
  pack
end