require 'rvg/rvg'
include Magick
RVG::dpi = 72

# count number of '1'
def onemax(bitstring)
  sum = 0
  bitstring.size.times {|i| sum+=1 if bitstring[i].chr=='1'}
  return sum
end

# generate a string of length num_bits
def random_bitstring(num_bits)
  return (0...num_bits).inject(""){|s,i| s<<((rand<0.5) ? "1" : "0")}
end

# pick 2 random but *distinct* candidates, pick the one with higher fitness
def binary_tournament(pop)
  i, j = rand(pop.size), rand(pop.size)
  j = rand(pop.size) while j==i
  return (pop[i][:fitness] > pop[j][:fitness]) ? pop[i] : pop[j]
end

# Randomly mutate DNA.
# DNA's length = # of times to attempt mutation.
# rate = 1/(DNA's length), so longer strand, lower mutation rate
def point_mutation(bitstring, rate=1.0/bitstring.size)
  # print "*"		# YKY added
  child = ""
   bitstring.size.times do |i|
     bit = bitstring[i].chr
     child << ((rand() < rate) ? ((bit=='1') ? "0" : "1") : bit)
  end
  return child
end

# Pick a point within Parent1,
# cross Parent1's DNA with Parent2's
def crossover(parent1, parent2, rate)
  # print "." # added by YKY
  return ""+parent1 if rand() >= rate
  point = 1 + rand(parent1.size - 2)
  return parent1[0...point] + parent2[point...(parent1.size)]
end

# Reproduce for 1 generation
def reproduce(selected, pop_size, p_cross, p_mutation)
  children = []
  selected.each_with_index do |p1, i|
    # p2 = (i.modulo(2)==0) ? selected[i+1] : selected[i-1]
    # p2 = selected[0] if i == selected.size-1
    p2 = selected[rand(selected.size)]
    child = {}
    child[:bitstring] = crossover(p1[:bitstring], p2[:bitstring], p_cross)
    child[:bitstring] = point_mutation(child[:bitstring], p_mutation)
    children << child
    break if children.size >= pop_size
  end
  return children
end

if __FILE__ == $0
  # problem configuration
  num_bits = 64
  # algorithm configuration
  max_gens = 100
  pop_size = 100
  p_crossover = 0.98
  p_mutation = 1.0/num_bits

  #$population = Array.new(pop_size)
  $population = Array.new(pop_size) do |i|
    {:bitstring=>random_bitstring(num_bits)}
  end

end

$rvg = RVG.new(540.px, 820.px).viewbox(0,0,540,820) do |canvas|
    canvas.background_fill = 'black'

    #canvas.g.translate(100, 100) do |body|
        #body.styles(:fill=>'green', :stroke=>'white', :stroke_width=>2)
        #body.ellipse(50, 30)
        #body.rect(45, 20, -20, -10).skewX(-35)
    #end

	x = 0
	y = 0
	$population.each do |p|
		y += 8
		x = 0
		dna = p[:bitstring]
		dna.size.times do |i|
		  x += 8
		  bit = dna[i].chr
		  if bit == '1'
			canvas.g.translate(x, y) do |test|
			test.styles(:fill=>'yellow', :stroke=>'red', :stroke_width=>2)
			test.rect(8, 8, 0, 0)
			end
		  end
		end
	end

    #canvas.g.translate(100, 150).rotate(-30) do |body|
        #body.styles(:fill=>'green', :stroke=>'black', :stroke_width=>2)
        #body.ellipse(50, 30)
        #body.rect(45, 20, -20, -10).skewX(-35)
    #end

    #canvas.g.translate(130, 83) do |head|
        #head.styles(:stroke=>'black', :stroke_width=>2)
        #head.circle(30).styles(:fill=>'yellow')
        #head.circle(5, 10, -5).styles(:fill=>'black')
        #head.polygon(30,0, 70,5, 30,10, 62,25, 23,20).styles(:fill=>'orange')
    #end

    #foot = RVG::Group.new do |_foot|
        #_foot.path('m0,0 v30 l30,10 l5,-10, l-5,-10 l-30,10z').
              #styles(:stroke_width=>2, :fill=>'orange', :stroke=>'black')
    #end
    #canvas.use(foot).translate(75, 188).rotate(15)
    #canvas.use(foot).translate(100, 185).rotate(-15)

    #canvas.text(125, 30) do |title|
        #title.tspan("duck|").styles(:text_anchor=>'end', :font_size=>20,
                       #:font_family=>'helvetica', :fill=>'black')
        #title.tspan("type").styles(:font_size=>22,
               #:font_family=>'times', :font_style=>'italic', :fill=>'red')
    #end
end

# Main algorithm
def search(max_gens, num_bits, pop_size, p_crossover, p_mutation)
  ## Initialize population
  population = Array.new(pop_size) do |i|
    {:bitstring=>random_bitstring(num_bits)}
  end
  
  # Calculate fitness for each candidate
  population.each{|c| c[:fitness] = onemax(c[:bitstring])}
  # Sort, and find best candidate
  best = population.sort{|x,y| y[:fitness] <=> x[:fitness]}.first
  
  # main loop
  max_gens.times do |gen|
	# tournament picks 2 random fighters and returns 1 winner each time
    selected = Array.new(pop_size){|i| binary_tournament(population)}
    # reproduce for 1 generation
    children = reproduce(selected, pop_size, p_crossover, p_mutation)
    children.each{|c| c[:fitness] = onemax(c[:bitstring])}
    children.sort!{|x,y| y[:fitness] <=> x[:fitness]}
    # children.each{|c| printf "child: %s\n", c[:bitstring]}
    best = children.first if children.first[:fitness] >= best[:fitness]
    population = children
    printf " > gen %02d, best: #{best[:fitness]}, #{best[:bitstring]}\n", gen
    break if best[:fitness] == num_bits
    
	rvg = RVG.new(540.px, 820.px).viewbox(0,0,540,820) do |canvas|
		canvas.background_fill = 'black'

		x = 0
		y = 0
		population.each do |p|
			y += 8
			x = 0
			dna = p[:bitstring]
			dna.size.times do |i|
			  x += 8
			  bit = dna[i].chr
			  if bit == '1'
				canvas.g.translate(x, y) do |test|
				test.styles(:fill=>'yellow', :stroke=>'red', :stroke_width=>2)
				test.rect(8, 8, 0, 0)
				end
			  end
			end
		end
	end
	
    img = rvg.draw()
	img.display
	# sleep(2)
  end
  return best
end

if __FILE__ == $0
  # execute the algorithm
  best = search(max_gens, num_bits, pop_size, p_crossover, p_mutation)
  puts "done! Solution:  #{best[:fitness]}, #{best[:bitstring]}"
end
