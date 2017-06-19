# frozen-string-literal: true

module Kademlia
  class Router
    attr_reader :source_node, :buckets

    def initialize(source_node)
      @source_node = source_node
      @buckets = [KBucket.new(0, 2 ** Kademlia::ID_LENGTH)]
    end

    def add_contact(node)
      index = find_bucket_for(node)
      bucket = buckets[index]

      return true if bucket.add_contact(node)

      if bucket.range.include?(source_node)
        split_bucket(index)
        add_contact(node)
      else
        puts "The node #{node.id} was discarded."
      end
    end

    def has_contact?(node)
      index = find_bucket_for(node)
      bucket = buckets[index]
      bucket.get_contact(node.id) && true if bucket
    end

    # TODO: Finish find_closest implementation.
    def find_closest(id)
      nodes = []
      buckets.each do |b|
        nodes = nodes + b.get_contacts
      end
      nodes
    end

    # def get_old_buckets
    #   buckets.select { |b| b.last_updated < (Time.now.to_i - 3600) }
    # end

    private

    def split_bucket(index)
      bucket = buckets[index]
      min, max = bucket.range.min, bucket.range.max
      mid = (min + max) / 2
      b1 = KBucket.new(min, mid)
      b2 = KBucket.new(mid + 1, max)
      bucket.contacts.values.each do |node|
        b = node.id_bignum <= mid ? b1 : b2
        b.add_node(node)
      end
      buckets[index] = b1
      buckets.insert(index + 1, b2)
    end

    # TODO: Review implementation.
    def find_bucket_for(node)
      # [ID_LENGTH - 1 - find_prefix(node), buckets.length - 1].min
      buckets.each.with_index do |bucket, index|
        return index if node.id_bignum < bucket.range.max
      end
    end

    # Alternative bucket finding implementation.
    def find_prefix(node)
      length = 0

      source_node.distance_to(node).bytes do |b|
        if x == 0
          length += 8
        else
          count = 0
          7.downto(1) do |i|
            (b & (1 << i)).zero? ? count += 1 : break
          end
          length += count
          break
        end
      end

      length
    end
  end
end
