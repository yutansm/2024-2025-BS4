export SSH_USER=b36156
export SSH_HOST=laurel.kudpc.kyoto-u.ac.jp
export SSH_DIR=/LARGE0/gr10291/nishimori2
export MPI_INC_DIR="/opt/homebrew/Cellar/open-mpi/5.0.5/include"
#export DYLD_LIBRARY_PATH=$(echo "$DYLD_LIBRARY_PATH" | awk -v RS=: -v ORS=: '!a[$0]++')


function ssh_login {
  ssh -i ~/京都くん.pem $SSH_USER@$SSH_HOST
}

# simplest
gfort() {
gfortran -ffree-line-length-none "$@"
}

# compiles with module that contains everdang
#gforty() {
#local DIR=/Users/yuta/LABWORK/2024-2025-BS4/aomori/Subroutines
#local LIB1=/opt/homebrew/Cellar/netcdf/4.9.2_2/lib
#gfortran -ffree-line-length-none -g -fcheck=all -I$DIR $DIR/module.o "$@" -L$LIB1 `nf-config --fflags --flibs`
#}

#function gforty {
#    local SR_DIR="/Users/yuta/LABWORK/2024-2025-BS4/aomori/Subroutines"
#    local OBJ_DIR="/Users/yuta/LABWORK/2024-2025-BS4/aomori/Subroutines"
#    local LIB1=/opt/homebrew/Cellar/netcdf/4.9.2_2/lib
#    mkdir -p "$OBJ_DIR"  # Ensure the object directory exists
#    for file in "$SR_DIR"/*.f90
#    do
#        local filename=$(basename "$file")
#        local namewoex="${filename%.*}"  # Remove the extension
#        local objfile="$OBJ_DIR/${namewoex}.o"
#
#        # Check if .o file doesn't exist or source file is newer
#        if [[ ! -f "$objfile" || "$file" -nt "$objfile" ]]; then
#            gfortran -ffree-line-length-none -c "$file" -o "$objfile"
#                if [ $? -eq 0 ]; then
#                    echo "Compiled [$filename] to [${namewoex}.o]"
#                else
#                    echo "Compilation of [$filename] to [${namewoex}.o] FAILED"
#                    return 1
#                fi
#        else
##            echo "SKIPPED $filename (not modified)"
#        fi
#    done
##    echo $OBJ_DIR
#        gfortran -ffree-line-length-none -g -fcheck=all -I$OBJ_DIR $OBJ_DIR/module.o "$@" -L$LIB1 `nf-config --fflags --flibs`
#    if [ $? -eq 0 ]; then
#        echo "Compiled [$(basename "$@")] as Source file"
#    else
#        echo "Compilation of [$(basename "$@")] failed"
#        return 1
#    fi
#    echo "Executed;"
#    ./a.out
#}
function gforty {
    local SR_DIR="/Users/yuta/LABWORK/2024-2025-BS4/aomori/Subroutines"
    local OBJ_DIR="/Users/yuta/LABWORK/2024-2025-BS4/aomori/Subroutines"
    local LIB1=/opt/homebrew/Cellar/netcdf/4.9.2_2/lib
    local output_name="a.out"  # Default output name
    local compile_args=()

    mkdir -p "$OBJ_DIR"  # Ensure the object directory exists
    for file in "$SR_DIR"/*.f90
    do
        local filename=$(basename "$file")
        local namewoex="${filename%.*}"  # Remove the extension
        local objfile="$OBJ_DIR/${namewoex}.o"
        
        # Check if .o file doesn't exist or source file is newer
        if [[ ! -f "$objfile" || "$file" -nt "$objfile" ]]; then
        echo "Compiling: [$filename] into an Object File"
            gfortran -ffree-line-length-none -c "$file" -o "$objfile"
            if [ $? -eq 0 ]; then
#                echo "Done"
            else
                echo "Compilation of [$filename] into [${namewoex}.o] FAILED"
                return 1
            fi
        fi
    done

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -o)
                output_name="$2"
                shift 2  # Skip the -o and its argument
                ;;
            *)
                compile_args+=("$1")
                shift
                ;;
        esac
    done

    # Compile with the specified output name
    echo "Compiling: [${compile_args[@]}] as Source File"
    gfortran -ffree-line-length-none -g -fcheck=all -I$OBJ_DIR $OBJ_DIR/module.o "${compile_args[@]}" -L$LIB1 `nf-config --fflags --flibs` -o "$output_name"

    if [ $? -eq 0 ]; then
#        echo "Done"
    else
        echo "Compilation of [${compile_args[@]}] Failed"
        return 1
    fi
    
    echo "Executing: [./$output_name]"
    ./"$output_name"
    if [ $? -eq 0 ]; then
        echo "Done."
    else
        echo "Execution of [${compile_args[@]}] Failed"
    return 1
    fi
}

function gfortc {
    local SR_DIR="/Users/yuta/LABWORK/2024-2025-BS4/aomori/Subroutines"
    local OBJ_DIR="/Users/yuta/LABWORK/2024-2025-BS4/aomori/Subroutines"
    local LIB1=/opt/homebrew/Cellar/netcdf/4.9.2_2/lib
#    mkdir -p "$OBJ_DIR"  # Ensure the object directory exists
    for file in "$SR_DIR"/*.f90
    do
        local filename=$(basename "$file")
        local namewoex="${filename%.*}"  # Remove the extension
        local objfile="$OBJ_DIR/${namewoex}.o"
        
        # Check if .o file doesn't exist or source file is newer
        if [[ ! -f "$objfile" || "$file" -nt "$objfile" ]]; then
            gfortran -ffree-line-length-none -c "$file" -o "$objfile"
            if [ $? -eq 0 ]; then
                echo "Compiled [$filename] to [${namewoex}.o]"
            else
                echo "Compilation of [$filename] to [${namewoex}.o] FAILED"
                return 1
            fi
#            echo "CONVERTED $filename TO ${namewoex}.o"
        else
#            echo "SKIPPED $filename (not modified)"
        fi
    done
}

function ps3pdf {
    local dir=${1:-.}
    for file in "$dir"/*.ps
    do
      ps2pdf "$file" "$dir/$(basename "${file%.ps}.pdf")"
    done
}

function pdfmerge {
    local dir=${1:-.}
    local output=${2:-merged.pdf}
    
    if [ ! -d "$dir" ]; then
        echo "Error: Directory '$dir' does not exist"
        return 1
    fi
    
    local files=("$dir"/*.pdf)
    if [ ${#files[@]} -eq 0 ]; then
        echo "Error: No PDF files found in directory '$dir'"
        return 1
    fi
    
    pdftk "$dir"/*.pdf cat output "$dir/$output"
    
    echo "Merged PDF files from '$dir' into '$output'"
}

function pspeed() {
    # Specify the directory to search
    local search_dir=/Users/yuta/LABWORK/2024-2025-BS4  # BS4 DIRECTORY ONLY!!

    # Find the newest .ps file
#    local newest_ps=$(find "$search_dir" -type f -name "*.ps" -print0 | xargs -0 ls -t | head -n1)
    local newest_ps=($search_dir/**/*.ps(om[1]))

    # Check if a .ps file was found
    if [[ -n $newest_ps ]]; then
        # Extract the directory and filename
        local dir=${newest_ps:h}
        local filename=${newest_ps:t}
        
        # Create the output PDF filename
        local output_pdf=${filename:r}.pdf
        
        # Convert the PS file to PDF
        ps2pdf "$newest_ps" "$dir/$output_pdf"
            if [ $? -eq 0 ]; then
                echo "CONVERTED $filename TO $output_pdf"
            else
                echo "CONVERSION OF $filename TO $output_pdf FAILED"
                return 1
            fi
    else
        print "No .ps files found in $search_dir or its subdirectories."
    fi
    # yey my first self-created command bro it's actually quite useful bro
}

delfiles() {
    local directory="$1"
    local extension=."$2"

    if [[ -z "$directory" || -z "$extension" ]]; then
        echo "Usage: delfiles <directory> <extension>"
        return 1
    fi

    if [[ ! -d "$directory" ]]; then
        echo "Error: '$directory' is not a valid directory"
        return 1
    fi

    # Find matching files
    local files=($(find "$directory" -maxdepth 1 -name "*$extension"))
    local file_count=${#files[@]}

    if (( file_count == 0 )); then
        echo "No file(s) with extension '$extension' found in '$directory'"
        return 0
    fi

    # List files subject to deletion
    echo "The following file(s) will be deleted:"
    for file in $files; do
        echo "  $(basename $file)"
    done

    # Ask for confirmation
    read "confirm?Are you sure you want to delete these $file_count file(s)? (Y/n) "
    if [[ "$confirm" == "Y" ]]; then
        rm "$directory"/*"$extension"
        echo "$file_count file(s) deleted."
    else
        echo "Operation cancelled."
    fi
}

