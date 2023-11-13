use std::process;
use std::env;


fn get_args(args: &Vec<String>) -> Result<(String, String), &'static str> {
    if args.len() < 3 {
        return Err("no file argument provided");
    }

    Ok((args[1].clone(), args[2].clone()))
}


fn main()
{
    let args: Vec<String> = env::args().collect();

    let (infile, _outfile) = get_args(&args).unwrap_or_else(|err| {
        eprintln!("{err}");
        process::exit(1);
    });

    cesco::compile(&infile);
    
    //cesco::transpile(&infile);
}