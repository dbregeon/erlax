require 'cucumber/rake/task'

namespace :cucumber do
    Cucumber::Rake::Task.new(:ok, 'Run validated features') do |t|
      t.fork = true # You may get faster startup if you set this to false
      t.profile = 'default'
    end
    
    Cucumber::Rake::Task.new(:wip, 'Run WIP features') do |t|
      t.fork = true # You may get faster startup if you set this to false
      t.profile = 'wip'
    end
end

desc 'Alias for cucumber:ok'
task :cucumber => 'cucumber:ok'