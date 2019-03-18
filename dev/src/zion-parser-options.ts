export class Options {
    public fromPath: boolean = false;

    constructor(fromPath?: boolean) {
        if (fromPath !== undefined) {
            this.fromPath = fromPath;
        }
    }
}