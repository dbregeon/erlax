require 'couchrest'

class CouchDB
  def initialize
	@databases = {}
  end

  def start
    @couchdb_node_pid = fork do
      exec("/Applications/CouchDBX.app/Contents/MacOS/CouchDBX")
    end
  end
  
  def database_for(name)
    return @databases[name] || Database.new(CouchRest.database!("http://127.0.0.1:5984/#{name}"))
  end
  
  def stop
    if !@couchdb_node_pid.nil?
      Process.kill "TERM", @couchdb_node_pid
      Process.wait @couchdb_node_pid
    end
  end
end
