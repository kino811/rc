# console-enter.ps1 - put one Enter key press into this console's input buffer.
#
# Used by claude-editor.cmd. When Claude Code (Windows) runs $EDITOR for Ctrl+G, a
# line-mode console read is still pending on the same console. Until it gets an Enter
# it takes every key (the console only echoes them), so terminal Emacs receives
# nothing, not even C-x or M-x. One Enter completes that read (Claude Code drops the
# line); after it Emacs gets every key.
#
# Do not use it where no such read is pending (a plain shell, herdr prefix+e): the
# Enter would then reach the editor as a blank line.
$source = @'
using System;
using System.Runtime.InteropServices;
public static class ConsoleEnter {
  [StructLayout(LayoutKind.Explicit, CharSet = CharSet.Unicode)]
  struct INPUT_RECORD {
    [FieldOffset(0)] public ushort EventType;
    [FieldOffset(4)] public int bKeyDown;
    [FieldOffset(8)] public ushort wRepeatCount;
    [FieldOffset(10)] public ushort wVirtualKeyCode;
    [FieldOffset(12)] public ushort wVirtualScanCode;
    [FieldOffset(14)] public char UnicodeChar;
    [FieldOffset(16)] public uint dwControlKeyState;
  }
  [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
  static extern IntPtr CreateFileW(string name, uint access, uint share, IntPtr sa, uint disposition, uint flags, IntPtr template);
  [DllImport("kernel32.dll", SetLastError = true)]
  static extern bool WriteConsoleInputW(IntPtr handle, INPUT_RECORD[] records, uint count, out uint written);
  [DllImport("kernel32.dll")]
  static extern bool CloseHandle(IntPtr handle);
  public static uint Send() {
    // CONIN$ is the console input buffer even when stdin is redirected.
    IntPtr h = CreateFileW("CONIN$", 0xC0000000, 3, IntPtr.Zero, 3, 0, IntPtr.Zero);
    if (h == new IntPtr(-1)) return 0;
    var records = new INPUT_RECORD[2];
    for (int i = 0; i < 2; i++) {
      records[i].EventType = 1;                 // KEY_EVENT
      records[i].bKeyDown = i == 0 ? 1 : 0;     // key down, then key up
      records[i].wRepeatCount = 1;
      records[i].wVirtualKeyCode = 0x0D;        // VK_RETURN
      records[i].wVirtualScanCode = 0x1C;
      records[i].UnicodeChar = '\r';
    }
    uint written;
    WriteConsoleInputW(h, records, 2, out written);
    CloseHandle(h);
    return written;
  }
}
'@
Add-Type -TypeDefinition $source
[void][ConsoleEnter]::Send()
# Give the pending read time to take the Enter before the editor starts reading.
Start-Sleep -Milliseconds 150
