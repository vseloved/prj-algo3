KADEMLIA_GEMSPEC = Gem::Specification.new do |s|
  s.platform = Gem::Platform::RUBY
  s.files = Dir['{spec,lib}/**/*.{rb,RB}']
  s.require_path = 'lib'
  s.bindir = 'bin'
  s.executables << 'kademlia'
end
