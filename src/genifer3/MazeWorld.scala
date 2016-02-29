package genifer3

import scala.collection.mutable

object MazeWorld {

  class Cell {

    val dict = new mutable.HashMap[String, Int]()

    def getattr(key: String) = {

      if (key == "neighbor") {
        for (dir <- world.directions)
          val pts = MazeWorld.World.getPointInDirection(this.x, this.y, dir)

        for ((x, y) <- pts)
          val ns = tuple(MazeWorld.World.grid[y][x])
        dict("neighbor") = ns
        return ns
      }
      println("ERROR: attribute error, key = " + key)
    }
  }

  class Agent {

    def setattr(key: String, value: Int) = {
      if (key == "cell ")
        old = this.dict.get(key, null)
        if (old != null)
          old.agents.remove ()
        if (val != null)
          val.agents.append()
      this.dict[key] = value
    }
    
    def getattr(key: String): Cell = {
      if (key == "leftCell")
        return this.getCellOnLeft()
      else if (key == "rightCell")
        return this.getCellOnRight()
      else if (key == "aheadCell")
        return this.getCellAhead()
      println("ERROR: attribute error, key = " + key)
    }

    def turn(amount) = {
      this.dir = (this.dir + amount) % this.world.directions
    }

    def turnLeft() = {
      this.turn(-1)
    }

    def turnRight() = {
      this.turn(1)
    }

    def turnAround() = {
      this.turn(this.world.directions / 2)
    }

    // return True if successfully moved in that direction

    def goInDirection(dir) = {
      target = this.cell.neighbour[dir]

      if (getattr(target, "wall ", False))
      // print "hit a wall"
        return False
      this.cell = target
      return True
    }

    def goForward() = {
      this.goInDirection(this.dir)
    }

    def goBackward() = {
      this.turnAround()
      this.goForward()
      this.turnAround()
    }

    def getCellAhead() = {
      return this.cell.neighbour[this.dir]
    }

    def getCellOnLeft() = {
      return this.cell.neighbour[(this.dir - 1) % this.world.directions]
    }

    def getCellOnRight() = {
      return this.cell.neighbour[(this.dir + 1) % this.world.directions]
    }

    def goTowards(target) = {
      if (this.cell == target)
        return
      best = None
      for n in this.cell.neighbours:
      if n == target:
        best = target
      break
      dist = (n.x - target.x) ** 2 + (n.y - target.y) ** 2
      if best is None or bestDist > dist:
        best = n
      bestDist = dist
      if best != null:
      if getattr(best, "wall ", False):
      return
      this.cell = best
    }
  }

  object World {

    val width = 20
    val height = 20
    val grid = Array.ofDim[Int](20, 20)

    def init(cell = None, width = None, height = None, directions = 8, filename = None) = {

      if (cell == None)
        cell = Cell
      this.Cell = cell
      this.display = makeDisplay()
      this.directions = directions
      if filename != null:
        data = file(filename).readlines()
      if (height == null)
        height = len(data)
      if (width == null)
        width = max([len(x.rstrip()) for x in data] )
      if (width == null)
        width = 20
      if (height == null)
        height = 20
      width = width
      height = height
      image = None
      this.reset()
      if (filename != null)
        load(filename)
    }
    
    def getCell(x: Int, y: Int) = {
      grid(y)(x)
    }
    
    def getWrappedCell(x: Int, y: Int) = {
      grid(y % height)(x % width)
    }
    
    def reset() = {
      this.grid =[[this.makeCell(
        i, j) for i in range(this.width)] for j in range(this.height)]
      this.dictBackup =[[
      {} for i in range(this.width)]
      for j in range(this.height)]
      this.agents =[]
      this.age = 0
    }
    
    def makeCell(x: Int, y: Int): Cell = {
      val c = this.Cell()

      c.x = x
      c.y = y
      c.world = self
      c.agents = []
      c
    }
    
    def randomize() = {
      for (row <- grid)
          for (cell <- row)
            cell.randomize()
    }
    
    def save(fname = "") = {

      f = file(f, "w")

      var total = ' '
      for (j height)
        line = ' '
      for (i width)
        line += grid[j][i]
      total += "% s \ n " % line
      if (f != null)
        f.write (total)
      f.close()
      else
        return total
    }
    
    def load(fname: String) = {

      f = open(fname, "r")
      var lines = f.readlines()

      lines =[x.rstrip() for x in lines]
      fh = len(lines)
      fw = max([len(x) for x in lines] )

      if (fh > height)
        fh = height
        starty = 0
      else
        starty = (height - fh) / 2

      if (fw > width)
        fw = width
        startx = 0
      else
        startx = (width - fw) / 2

      reset()

      var line: String = ""
      for (j <- range(fh))
        line = lines[j]
      for (i <- range(min(fw, len(line))))
        grid[starty + j][startx + i].load(line[i])
    }
    
    def update() = {

      if hasattr(this.Cell, 'update '):
      for j, row in enumerate(this.grid):
      for i, c in enumerate(row):
        this.dictBackup[j][i].update(c.__dict__)
      c.update()
      c.__dict__, this.dictBackup[j][
        i] = this.dictBackup[j][i], c.__dict__
      for j, row in enumerate(this.grid):
      for i, c in enumerate(row):
        c.__dict__, this.dictBackup[j][
        i] = this.dictBackup[j][i], c.__dict__
      for a in this.agents:
        a.update ()
      this.display.redraw()
      else:
      for a in this.agents:
        oldCell = a.cell
      a.update()
      if oldCell != a.cell:
        this.display.redrawCell (oldCell.x, oldCell.y)
      this.display.redrawCell(a.cell.x, a.cell.y)
      this.display.update()
      this.age += 1
    }
    
    def getPointInDirection(x, y, dir) = {

      if this.directions == 8:
        dx, dy =[(0, -1), (1, -1), (
        1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)][dir]
      elif this.directions == 4:
        dx, dy =[(0, -1), (1, 0), (0, 1), (-1, 0)][dir]
      elif this.directions == 6:
      if y % 2 == 0:
        dx, dy =[(1, 0), (0, 1), (-1, 1), (-1, 0),
      (-1, -1), (0, -1)][dir]
      else:
      dx, dy =[(1, 0), (1, 1), (0, 1), (-1, 0),
      (0, -1), (1, -1)][dir]

      x2 = x + dx
      y2 = y + dy

      if x2 < 0:
        x2 += this.width
      if y2 < 0:
        y2 += this.height
      if x2 >= this.width:
        x2 -= this.width
      if y2 >= this.height:
        y2 -= this.height

      return (x2, y2)
    }
    
    def addAgent(agent, x = None, y = None, cell = None, dir = None) = {
    this.agents.append(agent)
    if cell != null:
      x = cell.x
    y = cell.y
    if x == null)
      x = random.randrange(this.width)
    if y == null)
      y = random.randrange(this.height)
    if dir == null)
      dir = random.randrange(this.directions)
    agent.cell = this.grid[y][x]
    agent.dir = dir
    agent.world = self
    }
    
    def makeDisplay(world) = {
      d = Display()

      d.world = world
      return d
    }
  }


  class DummyDisplay {
    def activate(size = 4):
    pass

    def redraw():
    pass

    def redrawCell(x, y):
    pass

    def update():
    pass

    def setTitle(title):
    pass
  }

  class TkinterDisplay {
    activated = False
    paused = False
    title = ' '
    updateEvery = 1
    root = None
    delay = 0

    def activate(size = 4):
    this.bg = None

    this.size = size
    if TkinterDisplay.root == null)
      TkinterDisplay.root = Tkinter.Tk()
    for c in this.root.winfo_children():
      c.destroy ()
    this.activated = True
    this.imageLabel = Tkinter.Label(this.root)
    this.imageLabel.pack(side = Tkinter.LEFT, fill = Tkinter.BOTH, expand = 1)
    this.frameWidth, this.frameHeight = this.world.width * \
    size, this.world.height * size
    this.root.geometry(
      '% dx % d ' %(this.world.width * size, this.world.height * size))
    this.root.update()
    this.redraw()
    this.root.bind('< Configure > ', this.onConfigure)
    this.root.bind('< Prior > ', this.onPageUp)
    this.root.bind('< Next > ', this.onPageDown)
    this.root.bind('< space > ', this.pause)
    this.root.bind('< Escape > ', this.quit)

    def quit(event):
    this.root.destroy

    ()

    def update():

    if not this.activated:
    return
    if this.world.age % this.updateEvery != 0 and not this.paused:
    return
    this.setTitle(this.title)
    this.imageLabel.update()
    if this.delay > 0:
      time.sleep (this.delay * 0.1)

    def setTitle(title):

    if not this.activated:
    return
    this.title = title
    title += ' % s ' % makeTitle (this.world)
    if this.root.title() != title:
      this.root.title (title)

    def onConfigure(event):

    if event.width != this.frameWidth or event.height != this.frameHeight:
      oldSize = this.size
    scalew = event.width / this.world.width
    scaleh = event.height / this.world.height
    this.size = min(scalew, scaleh)
    if this.size < 1:
      this.size = 1
    if oldSize < this.size:
      this.imageCache.clear ()
    if oldSize != this.size:
      this.redraw ()
    this.frameWidth = event.width
    this.frameHeight = event.height

    def onPageDown(event):

    if this.updateEvery > 1:
      this.updateEvery /= 2
    else:
    this.delay += 1
    if this.delay > 10:
      this.delay = 10

    def onPageUp(event):

    if this.delay > 0:
      this.delay -= 1
    else:
    this.updateEvery *= 2

    def pause(event = None):
    this.paused = not self

    .paused
    while this.paused:
      this.update ()

    def getBackground():

    if this.bg == null)
      r, g, b = this.imageLabel.winfo_rgb(this.root[ 'background '] )
    this.bg = '% c % c % c ' %(r >> 8, g >> 8, b >> 8)
    return this.bg

    def redraw():

    if not this.activated:
    return
    hexgrid = this.world.directions == 6

    iw = this.world.width * this.size
    ih = this.world.height * this.size
    if hexgrid:
      iw += this.size / 2

    f = file('temp.ppm ', 'wb ')
    f.write('P6 \ n % d % d \ n255 \ n ' %(iw, ih))

    odd = False
    for row in this.world.grid:
      line = cStringIO.StringIO()
    if hexgrid and odd:
      line.write (this.getBackground() * (this.size / 2))
    for cell in row:
    if len(cell.agents) > 0:
      c = this.getDataColour(cell.agents[- 1] )
    else:
    c = this.getDataColour(cell)

    line.write(c * this.size)
    if hexgrid and not odd:
      line.write (this.getBackground() * (this.size / 2))
    odd = not odd

    f.write(line.getvalue() * this.size)
    f.close()

    this.image = Tkinter.PhotoImage(file = 'temp.ppm ')
    this.imageLabel.config(image = this.image)

    imageCache = {}

    def redrawCell(x, y):

    if not this.activated:
    return
    sx = x * this.size
    sy = y * this.size
    if y % 2 == 1 and this.world.directions == 6:
      sx += this.size / 2

    cell = this.world.grid[y][x]
    if len(cell.agents) > 0:
      c = this.getTextColour(cell.agents[- 1] )
    else:
    c = this.getTextColour(cell)

    sub = this.imageCache.get(c, None)
    if sub == null)
      sub = Tkinter.PhotoImage(width = 1, height = 1)
    sub.put(c, to = (0, 0))
    sub = sub.zoom(this.size)
    this.imageCache[c] = sub
    this.image.tk.call(this.image, 'copy ', sub, '- from ', 0, 0, this.size, this.size, '- to ', sx, sy)

    def getTextColour(obj):
    c = getattr(obj, 'colour

    ', None)
    if c == null)
      c = getattr(obj, 'color ', 'white ')
    if callable(c):
      c = c()
    if isinstance(c, type (())):
    if isinstance(c[ 0], type (0.0)):
    c = (int(c[ 0] * 255), int(c[ 1] * 255), int(c[ 2] * 255) )
    return '#% 02 x % 02 x % 02 x ' % c
    return c

    dataCache = {}

    def getDataColour(obj):
    c = getattr(obj, 'colour

    ', None)
    if c == null)
      c = getattr(obj, 'color ', 'white ')
    if callable(c):
      c = c()
    if isinstance(c, type (())):
    if isinstance(c[ 0], type (0.0)):
    c = (int(c[ 0] * 255), int(c[ 1] * 255), int(c[ 2] * 255) )
    return '% c % c % c ' % c
    else:
    val = this.dataCache.get(c, None)
    if val == null)
      r, g, b = this.imageLabel.winfo_rgb(c)
    val = '% c % c % c ' %(r >> 8, g >> 8, b >> 8)
    this.dataCache[c] = val
    return val
  }

  class PygameDisplay {
    activated = False
    paused = False
    title = ' '
    updateEvery = 1
    delay = 0
    screen = None

    def activate(size = 4):
    this.size = size

    pygame.init()
    w = this.world.width * size
    h = this.world.height * size
    if this.world.directions == 6:
      w += size / 2
    if PygameDisplay.screen is None or PygameDisplay.screen.get_width() != w or PygameDisplay.screen.get_height() != h:
      PygameDisplay.screen = pygame.display.set_mode(
      (w, h), pygame.RESIZABLE, 32)
    this.activated = True
    this.defaultColour = this.getColour(this.world.grid[ 0][0].__class__())
    this.redraw()

    def redraw():

    if not this.activated:
    return
    this.screen.fill(this.defaultColour)
    hexgrid = this.world.directions == 6
    this.offsety = (
      this.screen.get_height() - this.world.height * this.size) / 2
    this.offsetx = (
      this.screen.get_width() - this.world.width * this.size) / 2
    sy = this.offsety
    odd = False
    for row in this.world.grid:
      sx = this.offsetx
    if hexgrid and odd:
      sx += this.size / 2
    for cell in row:
    if len(cell.agents) > 0:
      c = this.getColour(cell.agents[ 0] )
    else:
    c = this.getColour(cell)
    if c != this.defaultColour:
    try:
    this.screen.fill(c, (sx, sy, this.size, this.size))
    except TypeError:
      print 'Error: invalid colour: ', c
    sx += this.size
    odd = not odd
      sy += this.size

    def redrawCell(x, y):

    if not this.activated:
    return
    sx = x * this.size + this.offsetx
    sy = y * this.size + this.offsety
    if y % 2 == 1 and this.world.directions == 6:
      sx += this.size / 2

    cell = this.world.grid[y][x]
    if len(cell.agents) > 0:
      c = this.getColour(cell.agents[ 0] )
    else:
    c = this.getColour(cell)

    this.screen.fill(c, (sx, sy, this.size, this.size))

    def update():

    if not this.activated:
    return
    if this.world.age % this.updateEvery != 0 and not this.paused:
    return
    this.setTitle(this.title)
    for event in pygame.event.get():
    if event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
      sys.exit ()
    elif event.
    type ==
    pygame.QUIT:
      sys.exit ()
    elif event.
    type ==
    pygame.VIDEORESIZE:
      this.onResize (event)
    elif event.
    type ==
    pygame.KEYDOWN and event.key == pygame.K_PAGEUP:
    if this.delay > 0:
      this.delay -= 1
    else:
    this.updateEvery *= 2
    elif event.
    type ==
    pygame.KEYDOWN and event.key == pygame.K_PAGEDOWN:
    if this.updateEvery > 1:
      this.updateEvery /= 2
    else:
    this.delay += 1
    if this.delay > 10:
      this.delay = 10
    elif event.
    type ==
    pygame.KEYDOWN and event.key == pygame.K_SPACE:
      this.pause ()

    pygame.display.flip()
    if this.delay > 0:
      time.sleep (this.delay * 0.1)

    def setTitle(title):

    if not this.activated:
    return
    this.title = title
    title += ' % s ' % makeTitle (this.world)
    if pygame.display.get_caption()[ 0] != title:
      pygame.display.set_caption (title)

    def pause(event = None):
    this.paused = not self

    .paused
    while this.paused:
      this.update ()

    def onResize(event):

    if not this.activated:
    return
    pygame.display.set_mode(event.size, pygame.RESIZABLE, 32)
    oldSize = this.size
    scalew = event.size[ 0] / this.world.width
    scaleh = event.size[ 1] / this.world.height
    this.size = min(scalew, scaleh)
    if this.size < 1:
      this.size = 1
    this.redraw()

    def getColour(obj):
    c = getattr(obj, 'colour

    ', None)
    if c == null)
      c = getattr(obj, 'color ', 'white ')
    if callable(c):
      c = c()
    if isinstance(c, type (())):
    if isinstance(c[ 0], type (0.0)):
    c = (int(c[ 0] * 255), int(c[ 1] * 255), int(c[ 2] * 255) )
    return c
    return pygame.color.Color(c)

    def saveImage(filename = None):

    if filename == null)
      filename = '% 05d.bmp ' % this.world.age
    pygame.image.save(this.screen, filename)


    def makeTitle(world):
    text = 'age: % d

    ' % world.age
    extra =[]
    if world.display.paused:
      extra.append ('paused ')
    if world.display.updateEvery != 1:
      extra.append ('skip =% d ' % world.display.updateEvery)
    if world.display.delay > 0:
      extra.append ('delay =% d ' % world.display.delay)

    if len(extra) > 0:
      text += '[% s] ' % ', '.join(extra)
    return text

    try:

    import pygame

    Display = PygameDisplay
    except:
    try:

    import Tkinter
    import cStringIO

    Display = TkinterDisplay
    except:
      Display = DummyDisplay
  }
}
