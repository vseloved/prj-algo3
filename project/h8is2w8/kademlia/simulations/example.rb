include Kademlia

# First node
s1 = Server.new('1.1.1.1', 1025, '12b13e2dcd2dca62ca8bbe4358f3e82b3fa775c6')
s1.run

# Neigbors of s1
s2 = Server.new('2.2.2.2', 2000, '12b13e2dcd2dca62ca8bbe4358f3e82b3fa975c6')
s2.run
s2.bootstrap('1.1.1.1', 1025)

s3 = Server.new('3.3.3.3', 3000, '12b13e2dcd2dca62ca8bbe8378f3e82b3fa975c6')
s3.run
s3.bootstrap('1.1.1.1', 1025)

# Far away node
s4 = Server.new('4.4.4.4', 4000, '51354a4e35c62ed46e0937285b3ccf8869852d8dx')
s4.run
s4.bootstrap('2.2.2.2', 2000)
