class HelloWorld
  def self.hello(name)
    "Hello, #{name.empty? ? 'world' : name}!"
  end
end
