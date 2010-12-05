require "erb"

class Project < Object
    include FileTest

    def initialize(args)
        @project_name = args.shift
        if @project_name.nil?
            
        end
    end

    def scaffold()
        copy_directory(resources_path(), project_path())
    end
private
    def project_path
        return File.expand_path(Dir.pwd + '/' + @project_name)
    end

    def resources_path
        return File.expand_path(File.dirname(__FILE__) + '/../../../resources')
    end

    def item_path(source, item)
        return File.expand_path(source + '/' + item)
    end

    def copy_directory(source, destination)
        puts "Copying #{source} to #{destination}"
        if !exist?(destination)
            Dir.mkdir(destination)
        end
        Dir.foreach(source) {|item|
            item_destination = item_path(destination,item)
            item_source = item_path(source,item)
            if (directory?(item_source) and '.' != item and '..' != item)
                copy_directory(item_source, item_destination)
            elsif file?(item_source)
                copy(item_source, item_destination)
            end
        }
    end

    def copy(source, destination)
        puts "Instantiating template #{source} to #{destination}"
        erb = ERB.new(File.read(source))
        File.open(destination, 'w') { |file|
            file.print(erb.result(binding()))
        }
    end
end
