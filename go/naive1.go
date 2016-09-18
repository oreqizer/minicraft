package main

import (
	"fmt"
	"math"
	"time"
)

// Vector
// ---

type Vector struct {
	x, y, z float64
}

func (a Vector) Add(b Vector) Vector {
	return Vector{a.x + b.x, a.y + b.y, a.z + b.z}
}

func (a Vector) Sub(b Vector) Vector {
	return Vector{a.x - b.x, a.y - b.y, a.z - b.z}
}

func (a Vector) Mul(b Vector) Vector {
	return Vector{a.x * b.x, a.y * b.y, a.z * b.z}
}

func (a Vector) Dist(b Vector) float64 {
	s := a.Sub(b)
	return math.Sqrt(s.x*s.x + s.y*s.y + s.z*s.z)
}

// Block
// ---

const BlockTypes = 256

type BlockId int

type Block struct {
	loc        Vector // x, y, z within a chunk
	name       string
	durability int
	typ        BlockId
	textureid  int
	breakable  bool
	visible    bool
}

// Entity
// ---

type Type int

const (
	Zombie Type = iota
	Chicken
	Creeper
	Enderman
)

type Entity struct {
	loc   Vector // x, y, z within a chunk
	typ   Type
	name  string
	hp    int
	speed Vector
}

func (e *Entity) Move() {
	// Complex movement AI
	rngVector := Vector{1, 1, 1}
	moveVector := rngVector.Mul(e.speed)
	e.loc = e.loc.Add(moveVector)
}

func NewEntity(loc Vector, typ Type) *Entity {
	e := &Entity{
		loc: loc,
		typ: typ,
	}

	switch typ {
	case Zombie:
		e.name = "Zombie"
		e.hp = 50
		e.speed = Vector{0.5, 0, 0.5} // slow, can't fly

	case Chicken:
		e.name = "Chicken"
		e.hp = 25
		e.speed = Vector{0.75, 0.25, 0.75} // can fly a bit

	case Creeper:
		e.name = "Creeper"
		e.hp = 75
		e.speed = Vector{0.75, 0, 0.75}

	case Enderman:
		e.name = "Enderman"
		e.hp = 500
		e.speed = Vector{1, 1, 1} // does what he wants
	}

	return e
}

// Chunk
// ---

const (
	ChunkBlocks   = 65536
	ChunkEntities = 1000
)

type Chunk struct {
	loc      Vector // x, y, z within the world
	blocks   []BlockId
	entities []*Entity
}

func (c *Chunk) ProcessEntities() {
	for _, e := range c.entities {
		e.Move()
	}
}

func genBlockIds() []BlockId {
	ids := make([]BlockId, ChunkBlocks)
	for i := range ids {
		ids[i] = BlockId(i)
	}
	return ids
}

func genEntities() []*Entity {
	entities := make([]*Entity, 0, ChunkEntities)
	for i := 0; i < ChunkEntities/4; i++ {
		// Fancy procedural generation initial position equation
		f := float64(i)
		entities = append(entities, NewEntity(Vector{f, f, f}, Zombie))
		entities = append(entities, NewEntity(Vector{f + 1, f, f}, Chicken))
		entities = append(entities, NewEntity(Vector{f + 2, f, f}, Creeper))
		entities = append(entities, NewEntity(Vector{f + 3, f, f}, Enderman))
	}
	return entities
}

func NewChunk(loc Vector) *Chunk {
	return &Chunk{
		loc:      loc,
		blocks:   genBlockIds(),
		entities: genEntities(),
	}
}

// Game
// ---

const GameChunks = 100

type Game struct {
	chunks     []*Chunk
	blocks     []Block
	chunkCount int
	playerLoc  Vector
}

func (g *Game) LoadWorld() {
	for i := 0; i < GameChunks; i++ {
		g.chunks = append(g.chunks, NewChunk(Vector{float64(g.chunkCount), 0, 0}))
		g.chunkCount++
	}

	g.blocks = make([]Block, BlockTypes)
	for i := range g.blocks {
		f := float64(i)
		g.blocks[i] = Block{
			loc: Vector{f, f, f},
			name: fmt.Sprintf("Block:%d", i),
			durability: 100,
			typ: BlockId(i),
			textureid: 1,
			breakable: true,
			visible: true,
		}
	}
}

func (g *Game) UpdateChunks() {
	// remove chunks by index, duh
	rmChunks := make([]int, 2)
	for i, chunk := range g.chunks {
		if g.playerLoc.Dist(chunk.loc) > GameChunks {
			rmChunks = append(rmChunks, i)
		} else {
			chunk.ProcessEntities()
		}
	}

	for _, id := range rmChunks {
		// see https://github.com/golang/go/wiki/SliceTricks
		copy(g.chunks[:id], g.chunks[id+1:])
		g.chunks[len(g.chunks)-1] = NewChunk(Vector{float64(g.chunkCount), 0, 0})
		g.chunkCount++
	}
}

func newGame() *Game {
	return &Game{
		chunkCount: 0,
		playerLoc:  Vector{0, 0, 0},
	}
}

// main
// ---

func main() {
	game := newGame()
	fmt.Println("Loading World...")
	start := time.Now()
	game.LoadWorld()
	loadWorldTime := time.Since(start)
	fmt.Println("FINISHED!")
	fmt.Printf("Load Time: %s\n", loadWorldTime)
	// Game Loop, you can never leave
	frames := 0
	for {
		// check for dead entities
		start = time.Now()

		// mocking polling of the VR controller
		playerMov := Vector{0.1, 0, 0}
		game.playerLoc = game.playerLoc.Add(playerMov)
		game.UpdateChunks()

		t := time.Since(start)
		fmt.Println(frames, t)
		// Lock it at 60FPS
		if t < 16*time.Millisecond {
			time.Sleep(16*time.Millisecond - t)
		}

		frames++
	}
}
