require 'erlix'
require File.expand_path(File.dirname(__FILE__) + '/../lib/erlooms')
require File.expand_path(File.dirname(__FILE__) + '/../lib/couchdb')

# start the couchdb process
couchdb = CouchDB.new()
couchdb.start()
  
#start the erlang nodes (local and remote)
erlooms = Erlooms.new()
erlooms.start()
  
# wait for everything to be up
sleep(5)

Before do
  # Expose the objects to the steps
  @erlooms = erlooms
  @couchdb = couchdb
end

at_exit do
  #cleanup
  erlooms.stop()
  couchdb.stop()
end

