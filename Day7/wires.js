#!/usr/bin/env node

const FS      = require("fs");
const Signage = require("signage");
const uint16  = Signage.short.unsigned;

const Cache = {};

class Instruction {
    constructor(outputName) {
        this.outputName = outputName;
    }

    cacheOrExec(allInstructions) {
        if (this.outputName && this.outputName in Cache) {
            return Cache[this.outputName];
        }

        const result = this.exec(allInstructions);

        Cache[this.outputName] = result;

        return result;
    }

    static fromText(text) {
        const [input, outputName] = text.split(" -> ");
        let matches;
        let instruction;

        if (input.match(/^(\d+|\S+)$/)) {
            instruction = new Assign(outputName, input);
        } else if (matches = input.match(/^NOT\s+(\S+)/)) {
            instruction = new Not(outputName, matches[1]);
        } else if (matches = input.match(/^(\S+)\s+AND\s+(\S+)/)) {
            instruction = new And(outputName, matches[1], matches[2]);
        } else if (matches = input.match(/^(\S+)\s+OR\s+(\S+)/)) {
            instruction = new Or(outputName, matches[1], matches[2]);
        } else if (matches = input.match(/^(\S+)\s+LSHIFT\s+(\S+)/)) {
            instruction = new LShift(outputName, matches[1], matches[2]);
        } else if (matches = input.match(/^(\S+)\s+RSHIFT\s+(\S+)/)) {
            instruction = new RShift(outputName, matches[1], matches[2]);
        } else {
            instruction = new NOP(outputName);
        }

        return instruction;
    }
}

class NOP extends Instruction {
    exec() {
        throw new Error("Not implemented");
    }
}

class RawNumber extends Instruction {
    constructor(number) {
        super();
        this.number = number;
    }

    exec() {
        return uint16(this.number);
    }
}

class Assign extends Instruction {
    constructor(outputName, instructionName) {
        super(outputName);
        this.instructionName = instructionName;
    }

    exec(allInstructions) {
        return allInstructions.exec(this.instructionName);
    }
}

class Not extends Instruction {
    constructor(outputName, instructionName) {
        super(outputName);
        this.instructionName = instructionName;
    }

    exec(allInstructions) {
        return uint16(~(allInstructions.exec(this.instructionName)));
    }
}

class And extends Instruction {
    constructor(outputName, instructionNameA, instructionNameB) {
        super(outputName);
        this.instructionNameA = instructionNameA;
        this.instructionNameB = instructionNameB;
    }

    exec(allInstructions) {
        return uint16(allInstructions.exec(this.instructionNameA) & allInstructions.exec(this.instructionNameB));
    }
}

class Or extends Instruction {
    constructor(outputName, instructionNameA, instructionNameB) {
        super(outputName);
        this.instructionNameA = instructionNameA;
        this.instructionNameB = instructionNameB;
    }

    exec(allInstructions) {
        return uint16(allInstructions.exec(this.instructionNameA) | allInstructions.exec(this.instructionNameB));
    }
}

class LShift extends Instruction {
    constructor(outputName, instructionNameA, instructionNameB) {
        super(outputName);
        this.instructionNameA = instructionNameA;
        this.instructionNameB = instructionNameB;
    }

    exec(allInstructions) {
        return uint16(allInstructions.exec(this.instructionNameA) << allInstructions.exec(this.instructionNameB));
    }
}

class RShift extends Instruction {
    constructor(outputName, instructionNameA, instructionNameB) {
        super(outputName);
        this.instructionNameA = instructionNameA;
        this.instructionNameB = instructionNameB;
    }

    exec(allInstructions) {
        return uint16(allInstructions.exec(this.instructionNameA) >> allInstructions.exec(this.instructionNameB));
    }
}

class Instructions extends Array {
    get(name) {
        const instruction = this.find(instruction => {
            return instruction.outputName === name;
        });

        if (instruction) {
            return instruction;
        }

        const matches = name.match(/^(\d+)$/);

        if (matches) {
            return new RawNumber(parseInt(matches[1]));
        }

        throw new Error(`No entry for '${name}' found`);
    }

    exec(name) {
        return this.get(name).cacheOrExec(this);
    }
}

FS.readFile("input", "utf8", function(error, contents) {
    if (error) {
        return;
    }

    const lines = contents.split("\n").filter(line => {
        return line !== null;
    });

    const instructions = Instructions.from(lines.map(line => {
        return Instruction.fromText(line);
    }));

    const instructionForB = instructions.get("b").instructionName = "956";
    const instructionForA = instructions.get("a");

    console.log(instructionForA.exec(instructions));
});
