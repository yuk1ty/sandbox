use std::{fs::File, io::Read, process::ExitCode};

// 2 でファイル読み込み時の失敗、3 でファイルが見つからなかったエラーを示すものとする。
fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();
    let file_path = &args[1];
    let file = File::open(file_path);
    match file {
        Ok(mut f) => {
            let mut content = String::new();
            let r = f.read_to_string(&mut content);
            if r.is_err() {
                return 2.into();
            }
            println!("{}", content);
            ExitCode::SUCCESS
        }
        Err(_) => 3.into(),
    }
}
