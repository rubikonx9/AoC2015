#!/usr/bin/env node

const FS = require("fs");

class Action {
    static fromText(text) {
        if (text.startsWith("turn on")) {
            return new TurnOn();
        }

        if (text.startsWith("turn off")) {
            return new TurnOff();
        }

        return new Toggle();
    }
}

class TurnOn extends Action {
    getValue() {
        return true;
    }
}

class TurnOff extends Action {
    getValue() {
        return false;
    }
}

class Toggle extends Action {
    getValue(value) {
        return !value;
    }
}

class Position {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
}

class Instruction {
    constructor(start, end, action) {
        this.start  = start;
        this.end    = end;
        this.action = action;
    }

    static fromText(text) {
        const action  = Action.fromText(text);
        const regex   = /(\d+),(\d+) through (\d+),(\d+)/;
        const matches = text.match(regex);

        if (!matches) {
            return [];
        }

        const start = new Position(parseInt(matches[1]), parseInt(matches[2]));
        const end   = new Position(parseInt(matches[3]), parseInt(matches[4]));

        return new Instruction(start, end, action);
    }
}

class Grid {
    static get SIZE() {
        return 1000;
    }

    constructor() {
        this.grid = new Array(Grid.SIZE);

        for (let i = 0; i < Grid.SIZE; ++i) {
            this.grid[i] = new Array(Grid.SIZE);

            for (let j = 0; j < Grid.SIZE; ++j) {
                this.grid[i][j] = false;
            }
        }
    }

    update(instruction) {
        for (let x = instruction.start.x; x <= instruction.end.x; ++x) {
            for (let y = instruction.start.y; y <= instruction.end.y; ++y) {
                this.grid[x][y] = instruction.action.getValue(this.grid[x][y]);
            }
        }
    }

    count() {
        return this.grid.reduce((sum, row) => {
            return sum + row.reduce((sum, cell) => {
                return sum + (cell === true);
            }, 0);
        }, 0);
    }
}

FS.readFile("input", "utf8", function(error, contents) {
    if (error) {
        return;
    }

    const lines = contents.split("\n").filter(line => {
        return line !== "";
    });

    const instructions = lines.map(line => {
        return Instruction.fromText(line);
    });

    const grid = new Grid();

    instructions.forEach(instruction => {
        grid.update(instruction);
    });

    console.log(grid.count());
});
