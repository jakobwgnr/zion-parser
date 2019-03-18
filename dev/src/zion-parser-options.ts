export class Options {
    public fromPath: boolean = true;

    constructor(fromPath?: boolean) {
        if (fromPath !== undefined) {
            this.fromPath = fromPath;
        }
    }
}