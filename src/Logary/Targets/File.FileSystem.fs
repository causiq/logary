namespace Logary.Targets.FileSystem

open System.IO

/// A folder path is a fully qualified path to a directory/folder; it's
/// should not be interpreted as a file and it should not just contain
/// a non-rooted path, i.e. it should be a rooted path.
type FolderPath = string

/// A file name does not contain a path/folder structure as part of its
/// contents. It's purely the name of the file and its extension (which is)
/// part of the file name.
type FileName = string

/// A file path is a fully qualified name of a file, in that it contains the
/// parent folders that the file is housed in.
type FilePath = string

/// How large the size is as judged from the file target (when a file is append)
/// mode its metadata may not reflect the accurate size.
type FileSize = int64

/// A regex that can be used to glob for files.
type FileNameRegex = System.Text.RegularExpressions.Regex

/// A file-system abstraction
type FileSystem =
  /// Gets a file in the current directory
  abstract getFile: FilePath -> FileInfo
  /// Moves a file to another location, within the current (chrooted)
  /// directory.
  abstract moveFile: FileName -> FileName -> unit
  /// Gets a sub-folder
  abstract getFolder: FolderPath -> DirectoryInfo
  /// Finds all files in the current folder (that the file system is chrooted
  /// to).
  abstract glob : (*file name*)FileNameRegex -> FileInfo seq
  /// Deletes the file passed as a parameter.
  abstract deleteFile: FilePath -> unit
  /// Chroot the file system to a given folder to avoid accidental deletions
  /// or modifications outside the folder of interest.
  abstract chroot: FolderPath -> FileSystem
  /// Ensures that the currently rooted path exists (mkdir -p)
  abstract ensureCurrent: unit -> DirectoryInfo

[<Sealed>]
type CountingStream(inner: Stream, written: int64 ref) =
  inherit Stream()

  /// Updates the passed bytes refrence
  member x.updateBytesRef () =
    written := inner.Position

  override x.Write(buffer, offset, length) =
    written := !written + int64 length
    inner.Write(buffer, offset, length)

  override x.WriteAsync(buffer, offset, length, cancellationToken) =
    written := !written + int64 length
    inner.WriteAsync(buffer, offset, length, cancellationToken)

  override x.BeginWrite(buffer, offset, count, callback, state) =
    written := !written + int64 count
    inner.BeginWrite(buffer, offset, count, callback, state)

  override x.EndWrite(state) =
    inner.EndWrite(state)

  // others:
  override x.CanRead = inner.CanRead
  override x.CanWrite = inner.CanWrite
  override x.CanSeek = inner.CanSeek
  override x.Length = inner.Length
  override x.Position
    with get() = inner.Position
    and set value = inner.Position <- value
  override x.Flush() = inner.Flush()
  override x.Seek (offset, origin) = inner.Seek (offset, origin)
  override x.SetLength value = inner.SetLength value
  override x.Read(buffer, offset, count) = inner.Read(buffer, offset, count)


/// This file system implementation contains the necessary code to use the
/// .Net System.IO abstractions.
[<Sealed>]
type DotNetFileSystem(root: FolderPath) =
  let ensureRooted (path: string) =
    if not (path.StartsWith root) then
      invalidOp (sprintf "Path '%s' is not within root '%s'" path root)
  let combine (segments: string seq) =
    Path.Combine(segments |> Array.ofSeq)

  let combEns (path: string) =
    let combined = combine [ root; path ]
    ensureRooted combined
    combined
  let getFolder (path: FolderPath) =
    let combined = combEns path
    DirectoryInfo combined

  interface FileSystem with
    member x.getFile path =
      let combined = combEns path
      FileInfo combined
    member x.moveFile fileName nextFileName =
      let source = combEns fileName
      let target = combEns nextFileName
      File.Move(source, target)
    member x.getFolder path =
      getFolder path
    member x.glob fnr =
      let info = getFolder root
      info.GetFiles()
      |> Array.filter (fun file -> fnr.IsMatch file.Name)
      |> Seq.ofArray
    member x.deleteFile filePath =
      ensureRooted filePath
      File.Delete filePath
    member x.chroot subPath =
      let combined = getFolder subPath
      DotNetFileSystem combined.FullName :> FileSystem
    member x.ensureCurrent () =
      Directory.CreateDirectory root

