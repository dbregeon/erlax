namespace :erlang do
  # No Need to change
  directory 'ebin'
  
  PWD = `pwd`.strip
  INCLUDE = "include"
  ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"
  
  SRC = FileList['src/**/*.erl']
  OBJ = SRC.pathmap("%{src,ebin}X.beam")
  APP = FileList['src/**/*.app'].pathmap("%{src,ebin}X.app")
  CLEAN.include(['**/*.dump', 'ebin/*.beam'])
  CLOBBER.include(['ebin/**/*.beam','ebin/**/*.app'])
  TEST = "-DNOTEST=true"
  
  rule ".beam" =>  ["%{ebin,src}X.erl"] do |t|
    sh "erlc #{TEST} -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source}"
  end
  
  rule /ebin\/(.+).app$/ => ["%{ebin,src}X.app"] do |t|
    copy(t.source,t.name)
  end
  
  desc "Compile all"
  task :compile => (['ebin'] + OBJ ) + (['ebin'] + APP)
  
  desc "Sets the TEST flag to include tests during the compilation"  
  task :with_test do
    TEST = "-DTEST=true +debug_info"
  end
  
  desc "Run Unit Tests"
  task :test => [:with_test, :compile] do
      puts "Modules under test:"
      OBJ.each do |obj|
        obj[%r{.*/(.*).beam}]
        mod = $1
        puts "#{mod}:\n"
        test_output = `erl -noshell -pa ebin -run cover compile_beam #{obj} -run cover start -run #{mod} test -run cover analyse #{mod} -run cover export '#{mod}.txt' -run init stop -noshell 1>&2`
    
        if /\*failed\*/ =~ test_output
          test_output[/(Failed.*Aborted.*Skipped.*Succeeded.*$)/]
        else
          test_output[/1>\s*(.*)\n/]
        end
      end
    end
  
  
  desc "Generate Documentation"
  task :doc do
      sh("cd doc && erl -noshell -run edoc files ../#{SRC.join(" ../")} -run init stop")
  end
end

desc 'Alias for erlang:compile'
task :erlang => 'erlang:compile'