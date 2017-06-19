# frozen-string-literal: true

module Kademlia
  class KBucket
    attr_reader :contacts, :cache, :range

    def initialize(range_min, range_max)
      @range = Range.new(range_min, range_max)
      @contacts = {}
      @cache = [] # Replacement cache is kept sorted by time last seen
      @last_updated = Time.now.to_i
    end

    def add_contact(node)
      if contacts.key?(node.id)
        contacts.delete(node.id)
        contacts[node.id] = node
      elsif contacts.size < Kademlia::K
        contacts[node.id] = node
      else
        cache << node
        return false
      end
      true
    end

    def get_contact(id)
      contacts[id]
    end

    def get_contacts
      contacts.values
    end
  end
end
