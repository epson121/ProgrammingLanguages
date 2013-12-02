class MyPiece < Piece

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece(board)
  	MyPiece.new(CheatPiece, board)
  end
   # class array holding all the pieces and their rotations
  CheatPiece = [[[0, 0]]]
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                [[0, 0], [0, -1], [0, 1], [0, 2]]],
               [[[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]], # long x5
                [[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]]],
               [[[0, 0], [1, 0], [0, 1], [1, 1], [1, 2]], # square + one
               	[[0, 0], [1, 0], [0, 1], [1, 1], [2, 0]],
               	[[0, 0], [1, 0], [0, 1], [1, 1], [0, -1]],
               	[[0, 0], [1, 0], [0, 1], [1, 1], [-1, 1]]],
               [[[0, 0], [1, 0], [1, 1]],[[0, 0], [1, 0], [0, 1]],
                [[0, 0], [1, 1], [0, 1]],[[1, 1], [1, 0], [0, 1]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]])] # Z
end

class MyBoard < Board
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
  end

  def next_piece()
  	if @cheat
  		@current_block = MyPiece.cheat_piece(self)
  		@cheat = false
  	else
    	@current_block = MyPiece.next_piece(self)
  	end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size-1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def rotate_180_deg
  	rotate_clockwise
  	rotate_clockwise
  end

  def cheat
  	if @score >= 100 and !@cheat
  		@score -= 100
  		@cheat = true
  	end
  end
end

class MyTetris < Tetris
  
  def initialize
    super
  end

  def set_board
  	@canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings  
    super
    @root.bind('c', proc {@board.cheat})
    @root.bind('u', proc {@board.rotate_180_deg})
  end
end


