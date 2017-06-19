# frozen-string-literal: true

module Kademlia
  class Protocol
    # TODO: In all RPCs, the recipient must echo 160-bit random RPC ID
    # to provide some resistance to address forgery.
    def initialize(source_node)
      @source_node = source_node
      @router = Router.new(source_node)
    end

    # Mock PING RPC.
    def ping(ip, port)
      message = { id: source_node.id, type: 'PING' }
      Kademlia.servers[ip].listen(message)
    end

    def store
      raise NotImplementedError
    end

    # Mock FIND_NODE RPC.
    def find_node(src, dst)
      check_node(src)
      message = {
        key: dst.id,
        type: 'FIND_NODE',
        sender: {
          ip: source_node.ip,
          port: source_node.port,
          id: source_node.id
        }
      }
      Kademlia.servers[src.ip].listen(message)
    end

    def find_value
      raise NotImplementedError
    end

    # Mock answer to PING RPC.
    def receive_ping
      { status: 'ok', receiver_id: source_node.id }
    end

    # Mock answer to FIND_NODE RPC.
    def receive_find_node(message)
      # Check sender node
      sender = message[:sender]
      node = Node.new(sender[:ip], sender[:port], sender[:id])
      check_node(node)
      # Collect closest nodes.
      router.find_closest(message[:key])
    end

    private

    attr_reader :source_node, :router

    def check_node(node)
      router.add_contact(node) if !router.has_contact?(node)
    end
  end
end
