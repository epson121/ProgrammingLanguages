class A

	def initialize(f=0)
		@foo = f
	end

	def m1
		@foo = 0
	end

	def m2 x
		@foo += x
	end

	def foo
		@foo
	end
end
x = A.new(5)
p x.foo
=begin
x = A.new
y = A.new
z = x
p x.foo
p x.m1
p z.foo
p y.foo
p z.m2 17
p x.m2 14
p z.foo
p x.m1
p z.foo
p y.foo
p y.m1
p y.m2 7
p y.foo
=end

