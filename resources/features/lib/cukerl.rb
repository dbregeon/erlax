class Erlooms
  require 'erlix'

  def start
    start_erlang_node
    connection.rpc("application","start",ErlixList.new([ErlixAtom.new("erlooms")]))
  end
  
  def stop
    if !@erlang_node_pid.nil?
      Process.kill "TERM", @erlang_node_pid
      Process.wait @erlang_node_pid
    end
  end
  
  def process?(name)
    result = connection.rpc("global","whereis_name", ErlixList.new([ErlixAtom.new(name)]))
    wait_for_rpc_to_complete()
    return result
  end
  
private

def action(name, arguments)
  ErlixTuple.new([ErlixAtom.new(name), ErlixList.new(arguments)])
end

def wait_for_rpc_to_complete
  sleep(1)
end  

def start_erlang_node
  @erlang_node_pid = fork do
    exec("erl -pa ebin -sname test@localhost -setcookie test -run inets start -run appmon start -noshell")
  end if @erlang_node_pid.nil?
end

def connection
  if @connection.nil?
    ErlixNode.init("cucumber", "test")
    sleep(5)
    @connection = ErlixConnection.new("test@localhost")
  end
  return @connection 
end

end
