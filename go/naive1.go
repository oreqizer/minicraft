package main

import (
	"math"
	"fmt"
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
	return math.Sqrt(s.x * s.x + s.y * s.y + s.z * s.z)
}

func newVector(x, y, z int) Vector {
	return Vector{float64(x), float64(y), float64(z)}
}

// Block
// ---

type Block struct {
	loc Vector // x, y, z within a chunk
	name string
	durability int
	typ int
	textureid int
	breakable bool
	visible bool
}

// Entity
// ---

type kind int

const (
	Zombie kind = iota
	Chicken
	Creeper
	Enderman
)

type Entity struct {
	loc Vector // x, y, z within a chunk
	name string
	typ kind
	hp int
	speed Vector
}

func (e Entity) Move() {
	// Complex movement AI
	rngVector := Vector{1, 1, 1}
	moveVector := rngVector.Mul(e.speed)
	e.loc = e.loc.Add(moveVector)
}

func newEntity(loc Vector, typ kind) Entity {
	switch (typ) {
	case Zombie:
		return Entity{
			loc: loc,
			name: "Zombie",
			typ: Zombie,
			hp: 50,
			speed: Vector{0.5, 0, 0.5}, // slow, can't fly
		}

	case Chicken:
		return Entity{
			loc: loc,
			name: "Chicken",
			typ: Chicken,
			hp: 25,
			speed: Vector{0.75, 0.25, 0.75}, // can fly a bit
		}

	case Creeper:
		return Entity{
			loc: loc,
			name: "Creeper",
			typ: Creeper,
			hp: 75,
			speed: Vector{0.75, 0, 0.75},
		}

	case Enderman:
		return Entity{
			loc: loc,
			name: "Enderman",
			typ: Enderman,
			hp: 500,
			speed: Vector{1, 1, 1}, // does what he wants
		}

	default:
		return Entity{}
	}
}

// Chunk
// ---

const (
	ChunkBlocks = 65536
	ChunkEntities = 1000
)

type Chunk struct {
	loc Vector // x, y, z within the world
	blocks []Block
	entities []Entity
}

func (c *Chunk) ProcessEntities() {
	for _, e := range c.entities {
		e.Move()
	}
}

func genBlocks() []Block {
	blocks := make([]Block, ChunkBlocks)
	for i := 0; i < ChunkBlocks; i++ {
		name := fmt.Sprintf("Block:%d", i)
		blocks = append(blocks, Block{newVector(i, i, i), name, 100, 1, 1, true, true})
	}
	return blocks
}

func genEntities() []Entity {
	entities := make([]Entity, ChunkEntities)
	for i := 0; i < ChunkEntities / 4; i++ {
		// Fancy procedural generation initial position equation
		entities = append(entities, newEntity(newVector(i, i, i), Zombie))
		entities = append(entities, newEntity(newVector(i + 1, i, i), Chicken))
		entities = append(entities, newEntity(newVector(i + 2, i, i), Creeper))
		entities = append(entities, newEntity(newVector(i + 3, i, i), Enderman))
	}
	return entities
}

func newChunk(loc Vector) *Chunk {
	return &Chunk{
		loc: loc,
		blocks: genBlocks(),
		entities: genEntities(),
	}
}

// Game
// ---

const (
	GameChunks = 100
)

type Game struct {
	chunks []*Chunk
	chunkCount int
	playerLoc Vector
}

func (g *Game) LoadWorld() {
	for i := 0; i < GameChunks; i++ {
		g.chunks = append(g.chunks, newChunk(newVector(g.chunkCount, 0, 0)))
		g.chunkCount++
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
		copy(g.chunks[:id], g.chunks[id + 1:])
		g.chunks[len(g.chunks) - 1] = newChunk(newVector(g.chunkCount, 0, 0))
		g.chunkCount++
	}
}

func newGame() *Game {
	return &Game{
		chunks: make([]*Chunk, GameChunks),
		chunkCount: 0,
		playerLoc: Vector{0, 0, 0},
	}
}

// main
// ---

func main() {
	game := &Game{}
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