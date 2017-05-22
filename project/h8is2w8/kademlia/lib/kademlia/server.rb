# frozen-string-literal: truek

require 'kademlia/core'

module Kademlia
  # TODO: Remove .servers when network layer will be ready.
  def self.servers
    @@servers ||= {}
  end

  class Server
    attr_reader :source_node, :protocol

    def initialize(ip, port, id = nil)
      @source_node = Node.new(ip, port, id)
      @protocol = Protocol.new(@source_node)
    end

    # Mock server activity.
    def run
      puts "Server with id = #{source_node.id} was started."
      Kademlia.servers[source_node.ip] = self
    end

    # Mock listening for incomming messages from other nodes.
    def listen(message)
      case message[:type]
      when 'PING'
        protocol.receive_ping
      when 'FIND_NODE'
        protocol.receive_find_node(message)
      end
    end

    def bootstrap(ip, port)
      # 1. Ping address of participating node to check
      #    it availability and obtain its ID.
      response = protocol.ping(ip, port)

      # 2. Start nodes lookup for our own ID to get nodes close to us.
      #    Perform look until no closer nodes are found.
      if response[:status] == 'ok'
        new_node = Node.new(ip, port, response[:receiver_id])
        # TODO: Sort nodes by XOR-metric in acs order
        # TODO: Pick ALPHA closest nodes and repeat node lookup.
        nodes = protocol.find_node(new_node, source_node)
        nodes.each do |n|
          nodes + protocol.find_node(n, source_node)
        end
        puts 'Bootstrap is over'
      end
    end

    # Obtain peers of specific ID
    def find(id)
      raise NotImplementedError
    end
  end
end
