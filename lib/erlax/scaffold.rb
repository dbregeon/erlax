module Erlax
    class Scaffold < Object
        def run(arguments)
            command_name = arguments.shift
	    if !command_name.nil?
                begin
	            require  File.expand_path(File.dirname(__FILE__) + '/commands/' + command_name)
	            command = eval("#{command_name.capitalize}.new(arguments)")
                    command.scaffold
                rescue Exception => e
                    puts e
                    usage("unknown command specified")
                end
            else
                usage("no command specified")
            end
	end

        def usage(reason)
             puts "#{reason}\n\n"
             puts <<-USAGE
scaffold enables to generate scaffolding for a specific topic.

Usage: erlax scaffold <topic> <name>

Examples:
    erlax scaffold project my_project
    erlax scaffold feature my_awesome_feature
    erlax scaffold entity my_business_entity
    erlax scaffold server my_service_server

USAGE
        end
    end
end
