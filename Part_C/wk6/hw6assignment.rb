# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
    # The constant All_My_Pieces should be declared here
    All_My_Pieces = [
      [[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
      rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
      [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
      [[0, 0], [0, -1], [0, 1], [0, 2]]],
      rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
      rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
      rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
      rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
      rotations([[0, 0], [0, 1], [1, 1]]), # corner
      rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]]), # 2x3
      [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2,0]], # 1x5
      [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]],
    ]
  
    # your enhancements here
    def self.next_piece (board)
      MyPiece.new(All_My_Pieces.sample, board)
    end

    def self.cheat_piece (board)
      MyPiece.new([[[0,0]]], board)
    end

    def area
      current_rotation.length
    end

  end
  
  class MyBoard < Board
    # your enhancements here
    attr_accessor :score, :cheat_pieces

    def initialize (game)
      super
      @current_block = MyPiece.next_piece(self)
      @block_area = @current_block.area
      @cheat_pieces = 0
      @score = 0
    end

    def next_piece
      if @cheat_pieces > 0
        @current_block = MyPiece.cheat_piece(self)
        @cheat_pieces -= 1
        @block_area = 1
      else
        @current_block = MyPiece.next_piece(self)
        @block_area = @current_block.area
      end
      @current_pos = nil
    end

    def store_current
      locations = @current_block.current_rotation
      displacement = @current_block.position
      (0...@block_area).each{|index| 
        current = locations[index];
        @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
        @current_pos[index]
      }
      remove_filled
      @delay = [@delay - 2, 80].max
    end
  
  end
  
  class MyTetris < Tetris
    # your enhancements here
    def key_bindings
      super
      @root.bind('u', proc {
        @board.rotate_clockwise
        @board.rotate_clockwise
      })
      @root.bind('c', proc {
        if @board.score >= 100
          @board.cheat_pieces += 1
          @board.score -= 100
        end
      })
    end

    def set_board
      @canvas = TetrisCanvas.new
      @board = MyBoard.new(self)
      @canvas.place(@board.block_size * @board.num_rows + 3,
                    @board.block_size * @board.num_columns + 6, 24, 80)
      @board.draw
    end

  
  end
  
  