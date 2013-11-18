class Hello 
	def my_first_method
		puts "Hello world"
	end
end

class A
	def m1
		34
	end

	def m2(x, y)
		z = 7
		if x > y
			z = -7
		end
		z
	end
end

instance = Hello.new
instance.my_first_method