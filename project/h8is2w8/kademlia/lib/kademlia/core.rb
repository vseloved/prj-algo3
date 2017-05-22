module Kademlia
  ID_LENGTH = 160 # A number of bits in key
  K = 20          # A number of possible nodes in a one bucket
  ALPHA = 3       # The quantity of simultaneous lookups

  def self.require(files, subdir = nil)
    Array(files).each do |f|
      super("#{File.dirname(__FILE__).untaint}/#{"#{subdir}/" if subdir}#{f}")
    end
  end

  require %w(node kbucket router protocol server)
end
