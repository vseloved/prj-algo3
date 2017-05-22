require 'digest'

module Kademlia
  class Node
    attr_reader :ip, :port, :id

    def initialize(ip, port, id = nil)
      @ip = ip
      @port = port
      # TODO: Implement better ID generation.
      @id = id || Digest::SHA1.hexdigest("#{ip}#{port}#{rand(9999)}")
    end

    def id_bignum
      @id_bignum ||= id.hex
    end

    def distance_to(other)
      id_bignum ^ other.id_bignum
    end
  end
end
