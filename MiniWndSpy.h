
/*****************************************************************************\
**                                                                           **
**  MiniWndSpy                                                               **
**                                                                           **
**  Licensed under the GNU General Public License v3.0 (the "License").      **
**  You may not use this file except in compliance with the License.         **
**  You can obtain a copy of the License at http://gnu.org/licenses/gpl-3.0  **
**                                                                           **
\*****************************************************************************/

#pragma warning(disable : 4706 /* assignment within conditional expression */)
#define _CRT_SECURE_NO_WARNINGS 1

#undef WINVER
#ifdef _WIN64
#define WINVER 0x501
#define _WIN32_IE 600
#else
#define WINVER 0x500
#define _WIN32_IE 0x400
#endif

#include <windows.h>
#include <windowsx.h>
#include <commctrl.h>
#include <uxtheme.h>
#include <tchar.h>
#include "MiniWndSpy.rc"

#ifndef WM_DPICHANGED
#define WM_DPICHANGED 0x02E0
#endif

#if defined(_MSC_VER) && _MSC_VER >= 1200
EXTERN_C IMAGE_DOS_HEADER __ImageBase;
#define HINST_APPLICATION ( (HINSTANCE) &__ImageBase )
#else
#define HINST_APPLICATION ( (HINSTANCE) GetModuleHandle(NULL) )
#endif

#if __cplusplus >= 201703L
#define CXX_ATT_FALLTHROUGH [[fallthrough]]
#else
#define CXX_ATT_FALLTHROUGH
#endif

#ifdef _WIN64
#define PRI_HWND TEXT("%.16X")
#else
#define PRI_HWND TEXT("%.8X")
#endif

BOOL (WINAPI*g_AdjustWindowRectExForDpi)(LPRECT pR, DWORD Style, BOOL Menu, DWORD ExStyle, UINT Dpi);
int (WINAPI*g_GetSystemMetricsForDpi)(UINT SM, UINT Dpi);
UINT (WINAPI*g_GetDpiForWindow)(HWND hwnd);
HRESULT (WINAPI*g_SetWindowTheme)(HWND,LPCWSTR,LPCWSTR);
HRESULT (WINAPI*g_GetDpiForMonitor)(HMONITOR hMonitor, UINT dpiType, UINT*dpiX, UINT*dpiY);
HRESULT (WINAPI*g_DwmGetWindowAttribute)(HWND hwnd,DWORD dwAttribute,PVOID pvAttribute,DWORD cbAttribute);

static inline UINT FASTCLASSHASH(LPCTSTR Str)
{
	return ((UINT*)(Str))[0] | 0x20202020;
}

static const LPCSTR g_HitTestStrings[] = {
	"HTERROR", "HTTRANSPARENT", "HTNOWHERE", "HTCLIENT", "HTCAPTION", "HTSYSMENU", 
	"HTGROWBOX/HTSIZE", "HTMENU", "HTHSCROLL", "HTVSCROLL", "HTMINBUTTON", 
	"HTMAXBUTTON", "HTLEFT", "HTRIGHT", "HTTOP", "HTTOPLEFT", "HTTOPRIGHT", "HTBOTTOM", 
	"HTBOTTOMLEFT", "HTBOTTOMRIGHT", "HTBORDER", "HTOBJECT", "HTCLOSE", "HTHELP"
};

#define MKSIMPLESTYLENAME(pre, name) ( /*pre ##*/ name )
#define MKSIMPLESTYLE(bits, pre, name) { (bits), MKSIMPLESTYLENAME(pre, name) }
struct SIMPLESTYLELUT { typedef LPCSTR STR; UINT Bit; LPCSTR Name; };
#define SIMPLESTYLELUT_SPECIALIZED_BEGIN(id) struct SimpleStyleLutHandler_##id : SimpleStyleLutHandlerInterface {
#define SIMPLESTYLELUT_SPECIALIZED_ENTRIES(LutName, Count) } LutName##Handler; static const SIMPLESTYLELUT LutName[] = { { (Count), (SIMPLESTYLELUT::STR) (&LutName##Handler) },
#define SIMPLESTYLELUT_ENTRIES(LutName, Count) static const SIMPLESTYLELUT LutName[] = { { (Count), (SIMPLESTYLELUT::STR) (&DefaultSimpleStyleLutHandler) },
#ifdef DEBUG
static const char DEBUG_SENTINEL[] = "DEBUG";
#define SIMPLESTYLELUT_END() , { 0xffffffffUL, DEBUG_SENTINEL } };
#else
#define VerifyStyles() ( (void*)(0) )
#define SIMPLESTYLELUT_END() };
#endif

struct DECLSPEC_NOVTABLE SimpleStyleLutHandlerInterface {
	typedef SIMPLESTYLELUT::STR STR;
	template<class T> static UINT GetCount(const T Lut[]) { return Lut[0].Bit; }
	static bool DefaultMatchHandler(const SIMPLESTYLELUT&E, UINT Bits, UINT) { return (E.Bit & Bits) == E.Bit; }
	virtual STR GetName(const SIMPLESTYLELUT&Entry, UINT WindowStyle) = 0;
	virtual bool Match(const SIMPLESTYLELUT&Entry, UINT Bits, UINT WindowStyle) = 0;
};

struct DefaultSimpleStyleLutHandler : SimpleStyleLutHandlerInterface {
	virtual STR GetName(const SIMPLESTYLELUT&E, UINT) { return E.Name; }
	virtual bool Match(const SIMPLESTYLELUT&E, UINT Bits, UINT WS) { return DefaultMatchHandler(E, Bits, WS); }
} DefaultSimpleStyleLutHandler;

static inline SimpleStyleLutHandlerInterface* GetHandler(const SIMPLESTYLELUT Lut[])
{
	SimpleStyleLutHandlerInterface *pHandler = (SimpleStyleLutHandlerInterface*) Lut[0].Name;
	return pHandler ? pHandler : &DefaultSimpleStyleLutHandler;
}

#define MKCCSSIMPLESTYLEMAP()                     \
	MKSIMPLESTYLE(0x0083, "CCS_", "RIGHT"),         \
	MKSIMPLESTYLE(0x0082, "CCS_", "NOMOVEX"),       \
	MKSIMPLESTYLE(0x0081, "CCS_", "LEFT"),          \
	MKSIMPLESTYLE(0x0080, "CCS_", "VERT"),          \
	MKSIMPLESTYLE(0x0040, "CCS_", "NODIVIDER"),     \
	MKSIMPLESTYLE(0x0020, "CCS_", "ADJUSTABLE"),    \
	MKSIMPLESTYLE(0x0010, "CCS_", "NOHILITE"),      \
	MKSIMPLESTYLE(0x0008, "CCS_", "NOPARENTALIGN"), \
	MKSIMPLESTYLE(0x0004, "CCS_", "NORESIZE"),      \
	MKSIMPLESTYLE(0x0003, "CCS_", "BOTTOM"),        \
	MKSIMPLESTYLE(0x0002, "CCS_", "NOMOVEY"),       \
	MKSIMPLESTYLE(0x0001, "CCS_", "TOP")

SIMPLESTYLELUT_ENTRIES(g_NoStyles, 0) };

SIMPLESTYLELUT_ENTRIES(g_ClassStyles, 13)
	MKSIMPLESTYLE(0x000001, "CS_", "VREDRAW"),
	MKSIMPLESTYLE(0x000002, "CS_", "HREDRAW"),
	MKSIMPLESTYLE(0x000008, "CS_", "DBLCLKS"),
	MKSIMPLESTYLE(0x000020, "CS_", "OWNDC"),
	MKSIMPLESTYLE(0x000040, "CS_", "CLASSDC"),
	MKSIMPLESTYLE(0x000080, "CS_", "PARENTDC"),
	MKSIMPLESTYLE(0x000200, "CS_", "NOCLOSE"),
	MKSIMPLESTYLE(0x000800, "CS_", "SAVEBITS"),
	MKSIMPLESTYLE(0x001000, "CS_", "BYTEALIGNCLIENT"),
	MKSIMPLESTYLE(0x002000, "CS_", "BYTEALIGNWINDOW"),
	MKSIMPLESTYLE(0x004000, "CS_", "GLOBALCLASS"),
	MKSIMPLESTYLE(0x010000, "CS_", "IME"),
	MKSIMPLESTYLE(0x020000, "CS_", "DROPSHADOW")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_SPECIALIZED_BEGIN(WS_)
	virtual STR GetName(const SIMPLESTYLELUT&E, UINT WS)
	{
		if (E.Bit == WS_THICKFRAME && !(WS & WS_CHILD)) return MKSIMPLESTYLENAME("WS_", "THICKFRAME");
		if (E.Bit == WS_MINIMIZEBOX && !(WS & WS_CHILD)) return MKSIMPLESTYLENAME("WS_", "MINIMIZEBOX");
		if (E.Bit == WS_MAXIMIZEBOX && !(WS & WS_CHILD)) return MKSIMPLESTYLENAME("WS_", "MAXIMIZEBOX");
		return E.Name;
	}
	virtual bool Match(const SIMPLESTYLELUT&E, UINT Bits, UINT WS) { return DefaultMatchHandler(E, Bits, WS); }
SIMPLESTYLELUT_SPECIALIZED_ENTRIES(g_WindowStyles, 19-3)
	//MKSIMPLESTYLE(WS_TILEDWINDOW, "WS_", "TILEDWINDOW"),
	//MKSIMPLESTYLE(WS_POPUPWINDOW, "WS_", "POPUPWINDOW"),
	//MKSIMPLESTYLE(WS_CAPTION,     "WS_", "CAPTION"),
	MKSIMPLESTYLE(0x80000000L, "WS_", "POPUP"),
	MKSIMPLESTYLE(0x40000000L, "WS_", "CHILD"),
	MKSIMPLESTYLE(0x20000000L, "WS_", "MINIMIZE"),
	MKSIMPLESTYLE(0x10000000L, "WS_", "VISIBLE"),
	MKSIMPLESTYLE(0x08000000L, "WS_", "DISABLED"),
	MKSIMPLESTYLE(0x04000000L, "WS_", "CLIPSIBLINGS"),
	MKSIMPLESTYLE(0x02000000L, "WS_", "CLIPCHILDREN"),
	MKSIMPLESTYLE(0x01000000L, "WS_", "MAXIMIZE"),
	MKSIMPLESTYLE(0x00800000L, "WS_", "BORDER"),
	MKSIMPLESTYLE(0x00400000L, "WS_", "DLGFRAME"),
	MKSIMPLESTYLE(0x00200000L, "WS_", "VSCROLL"),
	MKSIMPLESTYLE(0x00100000L, "WS_", "HSCROLL"),
	MKSIMPLESTYLE(0x00080000L, "WS_", "SYSMENU"),
	MKSIMPLESTYLE(0x00040000L, "WS_", "SIZEBOX"), // Also WS_THICKFRAME
	MKSIMPLESTYLE(0x00020000L, "WS_", "GROUP"),   // Also WS_MINIMIZEBOX
	MKSIMPLESTYLE(0x00010000L, "WS_", "TABSTOP")  // Also WS_MAXIMIZEBOX
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_WindowExStyles, 24-2+3)
	//MKSIMPLESTYLE(WS_EX_PALETTEWINDOW, "WS_EX_", "PALETTEWINDOW"),
	//MKSIMPLESTYLE(WS_EX_OVERLAPPEDWINDOW, "WS_EX_", "OVERLAPPEDWINDOW"),
	MKSIMPLESTYLE(0x00000001L, "WS_EX_", "DLGMODALFRAME"),
	MKSIMPLESTYLE(0x00000004L, "WS_EX_", "NOPARENTNOTIFY"),
	MKSIMPLESTYLE(0x00000008L, "WS_EX_", "TOPMOST"),
	MKSIMPLESTYLE(0x00000010L, "WS_EX_", "ACCEPTFILES"),
	MKSIMPLESTYLE(0x00000020L, "WS_EX_", "TRANSPARENT"),
	MKSIMPLESTYLE(0x00000040L, "WS_EX_", "MDICHILD"),
	MKSIMPLESTYLE(0x00000080L, "WS_EX_", "TOOLWINDOW"),
	MKSIMPLESTYLE(0x00000100L, "WS_EX_", "WINDOWEDGE"),
	MKSIMPLESTYLE(0x00000200L, "WS_EX_", "CLIENTEDGE"),
	MKSIMPLESTYLE(0x00000400L, "WS_EX_", "CONTEXTHELP"),
	MKSIMPLESTYLE(0x00001000L, "WS_EX_", "RIGHT"),
	MKSIMPLESTYLE(0x00002000L, "WS_EX_", "RTLREADING"),
	MKSIMPLESTYLE(0x00004000L, "WS_EX_", "LEFTSCROLLBAR"),
	MKSIMPLESTYLE(0x00010000L, "WS_EX_", "CONTROLPARENT"),
	MKSIMPLESTYLE(0x00020000L, "WS_EX_", "STATICEDGE"),
	MKSIMPLESTYLE(0x00040000L, "WS_EX_", "APPWINDOW"),
	MKSIMPLESTYLE(0x00080000L, "WS_EX_", "LAYERED"),
	MKSIMPLESTYLE(0x00100000L, "WS_EX_", "NOINHERITLAYOUT"),
	MKSIMPLESTYLE(0x00200000L, "WS_EX_", "NOREDIRECTIONBITMAP"),
	MKSIMPLESTYLE(0x00400000L, "WS_EX_", "LAYOUTRTL"),
	MKSIMPLESTYLE(0x02000000L, "WS_EX_", "COMPOSITED"),
	MKSIMPLESTYLE(0x08000000L, "WS_EX_", "NOACTIVATE"),
	MKSIMPLESTYLE(0x04000000L, "WS_EX_", "UIS_ACTIVE?"), // UISF_ACTIVE related? Set by (WM_UPDATEUISTATE, MAKELONG(UIS_SET, UISF_ACTIVE), 0).
	MKSIMPLESTYLE(0x40000000L, "WS_EX_", "UIS_HIDEACCEL?"), // UISF_HIDEACCEL related? Is removed when you press [ALT]. < Win10?
	MKSIMPLESTYLE(0x80000000L, "WS_EX_", "UIS_HIDEFOCUS?") // UISF_HIDEFOCUS related? Is removed when you change the selection using the keyboard. < Win10?
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_DialogStyles, 15)
	MKSIMPLESTYLE(DS_SHELLFONT, "DS_", "SHELLFONT"),
	MKSIMPLESTYLE(0x0001, "DS_", "ABSALIGN"),
	MKSIMPLESTYLE(0x0002, "DS_", "SYSMODAL"),
	MKSIMPLESTYLE(0x0004, "DS_", "3DLOOK"),
	MKSIMPLESTYLE(0x0008, "DS_", "FIXEDSYS"),
	MKSIMPLESTYLE(0x0010, "DS_", "NOFAILCREATE"),
	MKSIMPLESTYLE(0x0020, "DS_", "LOCALEDIT"),
	MKSIMPLESTYLE(0x0040, "DS_", "SETFONT"),
	MKSIMPLESTYLE(0x0080, "DS_", "MODALFRAME"),
	MKSIMPLESTYLE(0x0100, "DS_", "NOIDLEMSG"),
	MKSIMPLESTYLE(0x0200, "DS_", "SETFOREGROUND"),
	MKSIMPLESTYLE(0x0400, "DS_", "CONTROL"),
	MKSIMPLESTYLE(0x0800, "DS_", "CENTER"),
	MKSIMPLESTYLE(0x1000, "DS_", "CENTERMOUSE"),
	MKSIMPLESTYLE(0x2000, "DS_", "CONTEXTHELP")
	//MKSIMPLESTYLE(0x8000, "DS_", "USEPIXELS"), // WinCE only?
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_SPECIALIZED_BEGIN(LVS_)
	virtual STR GetName(const SIMPLESTYLELUT&E, UINT WS)
	{
		if (E.Bit == 3 && (WS & 3) == 0) return MKSIMPLESTYLENAME("LVS_", "ICON");
		if (E.Bit == 0x0400 && (WS & LVS_REPORT)) return MKSIMPLESTYLENAME("LVS_", "OWNERDRAWFIXED");
		return E.Name;
	}
	virtual bool Match(const SIMPLESTYLELUT&E, UINT Bits, UINT WS)
	{
		return (Bits == 3 && (WS & 3) == 0) || DefaultMatchHandler(E, Bits, WS);
	}
SIMPLESTYLELUT_SPECIALIZED_ENTRIES(g_ListViewStyles, 18)
	MKSIMPLESTYLE(0x8000, "LVS_", "NOSORTHEADER"),
	MKSIMPLESTYLE(0x4000, "LVS_", "NOCOLUMNHEADER"),
	MKSIMPLESTYLE(0x2000, "LVS_", "NOSCROLL"),
	MKSIMPLESTYLE(0x1000, "LVS_", "OWNERDATA"),
	MKSIMPLESTYLE(0x0c00, "LVS_", "ALIGNRIGHT"),
	MKSIMPLESTYLE(0x0800, "LVS_", "ALIGNLEFT"),
	MKSIMPLESTYLE(0x0400, "LVS_", "ALIGNBOTTOM"), // LVS_OWNERDRAWFIXED in LVS_REPORT
	MKSIMPLESTYLE(0x0200, "LVS_", "EDITLABELS"),
	MKSIMPLESTYLE(0x0100, "LVS_", "AUTOARRANGE"),
	MKSIMPLESTYLE(0x0080, "LVS_", "NOLABELWRAP"),
	MKSIMPLESTYLE(0x0040, "LVS_", "SHAREIMAGELISTS"),
	MKSIMPLESTYLE(0x0020, "LVS_", "SORTDESCENDING"),
	MKSIMPLESTYLE(0x0010, "LVS_", "SORTASCENDING"),
	MKSIMPLESTYLE(0x0008, "LVS_", "SHOWSELALWAYS"),
	MKSIMPLESTYLE(0x0004, "LVS_", "SINGLESEL"),
	MKSIMPLESTYLE(0x0003, "LVS_", "LIST"),
	MKSIMPLESTYLE(0x0002, "LVS_", "SMALLICON"),
	MKSIMPLESTYLE(0x0001, "LVS_", "REPORT")
SIMPLESTYLELUT_END();
SIMPLESTYLELUT_ENTRIES(g_ListViewExStyles, 30)
	MKSIMPLESTYLE(0x80000000L, "LVS_EX_", "COLUMNOVERFLOW"),
	MKSIMPLESTYLE(0x40000000L, "LVS_EX_", "COLUMNSNAPPOINTS"),
	MKSIMPLESTYLE(0x10000000L, "LVS_EX_", "AUTOSIZECOLUMNS"),
	MKSIMPLESTYLE(0x08000000L, "LVS_EX_", "AUTOCHECKSELECT"),
	MKSIMPLESTYLE(0x02000000L, "LVS_EX_", "HEADERINALLVIEWS"),
	MKSIMPLESTYLE(0x01000000L, "LVS_EX_", "AUTOAUTOARRANGE"),
	MKSIMPLESTYLE(0x00800000L, "LVS_EX_", "TRANSPARENTSHADOWTEXT"),
	MKSIMPLESTYLE(0x00400000L, "LVS_EX_", "TRANSPARENTBKGND"),
	MKSIMPLESTYLE(0x00200000L, "LVS_EX_", "JUSTIFYCOLUMNS"),
	MKSIMPLESTYLE(0x00100000L, "LVS_EX_", "SIMPLESELECT"),
	MKSIMPLESTYLE(0x00080000L, "LVS_EX_", "SNAPTOGRID"),
	MKSIMPLESTYLE(0x00040000L, "LVS_EX_", "SINGLEROW"),
	MKSIMPLESTYLE(0x00020000L, "LVS_EX_", "HIDELABELS"),
	MKSIMPLESTYLE(0x00010000L, "LVS_EX_", "DOUBLEBUFFER"),
	MKSIMPLESTYLE(0x00008000L, "LVS_EX_", "BORDERSELECT"),
	MKSIMPLESTYLE(0x00004000L, "LVS_EX_", "LABELTIP"),
	MKSIMPLESTYLE(0x00002000L, "LVS_EX_", "MULTIWORKAREAS"),
	MKSIMPLESTYLE(0x00001000L, "LVS_EX_", "UNDERLINECOLD"),
	MKSIMPLESTYLE(0x00000800L, "LVS_EX_", "UNDERLINEHOT"),
	MKSIMPLESTYLE(0x00000400L, "LVS_EX_", "INFOTIP"),
	MKSIMPLESTYLE(0x00000200L, "LVS_EX_", "REGIONAL"),
	MKSIMPLESTYLE(0x00000100L, "LVS_EX_", "FLATSB"),
	MKSIMPLESTYLE(0x00000080L, "LVS_EX_", "TWOCLICKACTIVATE"),
	MKSIMPLESTYLE(0x00000040L, "LVS_EX_", "ONECLICKACTIVATE"),
	MKSIMPLESTYLE(0x00000020L, "LVS_EX_", "FULLROWSELECT"),
	MKSIMPLESTYLE(0x00000010L, "LVS_EX_", "HEADERDRAGDROP"),
	MKSIMPLESTYLE(0x00000008L, "LVS_EX_", "TRACKSELECT"),
	MKSIMPLESTYLE(0x00000004L, "LVS_EX_", "CHECKBOXES"),
	MKSIMPLESTYLE(0x00000002L, "LVS_EX_", "SUBITEMIMAGES"),
	MKSIMPLESTYLE(0x00000001L, "LVS_EX_", "GRIDLINES")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_TreeViewStyles, 16)
	MKSIMPLESTYLE(0x8000, "TVS_", "NOHSCROLL"),
	MKSIMPLESTYLE(0x4000, "TVS_", "NONEVENHEIGHT"),
	MKSIMPLESTYLE(0x2000, "TVS_", "NOSCROLL"),
	MKSIMPLESTYLE(0x1000, "TVS_", "FULLROWSELECT"),
	MKSIMPLESTYLE(0x0800, "TVS_", "INFOTIP"),
	MKSIMPLESTYLE(0x0400, "TVS_", "SINGLEEXPAND"),
	MKSIMPLESTYLE(0x0200, "TVS_", "TRACKSELECT"),
	MKSIMPLESTYLE(0x0100, "TVS_", "CHECKBOXES"),
	MKSIMPLESTYLE(0x0080, "TVS_", "NOTOOLTIPS"),
	MKSIMPLESTYLE(0x0040, "TVS_", "RTLREADING"),
	MKSIMPLESTYLE(0x0020, "TVS_", "SHOWSELALWAYS"),
	MKSIMPLESTYLE(0x0010, "TVS_", "DISABLEDRAGDROP"),
	MKSIMPLESTYLE(0x0008, "TVS_", "EDITLABELS"),
	MKSIMPLESTYLE(0x0004, "TVS_", "LINESATROOT"),
	MKSIMPLESTYLE(0x0002, "TVS_", "HASLINES"),
	MKSIMPLESTYLE(0x0001, "TVS_", "HASBUTTONS")
SIMPLESTYLELUT_END();
SIMPLESTYLELUT_ENTRIES(g_TreeViewExStyles, 11)
	MKSIMPLESTYLE(0x00000400L, "TVS_EX_", "DRAWIMAGEASYNC"),
	MKSIMPLESTYLE(0x00000200L, "TVS_EX_", "DIMMEDCHECKBOXES"),
	MKSIMPLESTYLE(0x00000100L, "TVS_EX_", "EXCLUSIONCHECKBOXES"),
	MKSIMPLESTYLE(0x00000080L, "TVS_EX_", "PARTIALCHECKBOXES"),
	MKSIMPLESTYLE(0x00000040L, "TVS_EX_", "FADEINOUTEXPANDOS"),
	MKSIMPLESTYLE(0x00000020L, "TVS_EX_", "AUTOHSCROLL"),
	MKSIMPLESTYLE(0x00000010L, "TVS_EX_", "RICHTOOLTIP"),
	MKSIMPLESTYLE(0x00000008L, "TVS_EX_", "NOINDENTSTATE"),
	MKSIMPLESTYLE(0x00000004L, "TVS_EX_", "DOUBLEBUFFER"),
	MKSIMPLESTYLE(0x00000002L, "TVS_EX_", "MULTISELECT"),
	MKSIMPLESTYLE(0x00000001L, "TVS_EX_", "NOSINGLECOLLAPSE")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_TabStyles, 16) // TODO: TCS_SINGLELINE, TCS_TABS, TCS_RIGHTJUSTIFY
	MKSIMPLESTYLE(0x8000, "TCS_", "FOCUSNEVER"),
	MKSIMPLESTYLE(0x4000, "TCS_", "TOOLTIPS"),
	MKSIMPLESTYLE(0x2000, "TCS_", "OWNERDRAWFIXED"),
	MKSIMPLESTYLE(0x1000, "TCS_", "FOCUSONBUTTONDOWN"),
	MKSIMPLESTYLE(0x0800, "TCS_", "RAGGEDRIGHT"),
	MKSIMPLESTYLE(0x0400, "TCS_", "FIXEDWIDTH"),
	MKSIMPLESTYLE(0x0200, "TCS_", "MULTILINE"),
	MKSIMPLESTYLE(0x0100, "TCS_", "BUTTONS"),
	MKSIMPLESTYLE(0x0080, "TCS_", "VERTICAL"),
	MKSIMPLESTYLE(0x0040, "TCS_", "HOTTRACK"),
	MKSIMPLESTYLE(0x0020, "TCS_", "FORCELABELLEFT"),
	MKSIMPLESTYLE(0x0010, "TCS_", "FORCEICONLEFT"),
	MKSIMPLESTYLE(0x0008, "TCS_", "FLATBUTTONS"),
	MKSIMPLESTYLE(0x0004, "TCS_", "MULTISELECT"),
	MKSIMPLESTYLE(0x0002, "TCS_", "RIGHT/BOTTOM"),
	MKSIMPLESTYLE(0x0001, "TCS_", "SCROLLOPPOSITE")
SIMPLESTYLELUT_END();
SIMPLESTYLELUT_ENTRIES(g_TabExStyles, 2)
	MKSIMPLESTYLE(0x00000002L, "TCS_EX_", "REGISTERDROP"),
	MKSIMPLESTYLE(0x00000001L, "TCS_EX_", "FLATSEPARATORS")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_ToolbarStyles, 8+12)
	MKSIMPLESTYLE(0x8000, "TBSTYLE_", "TRANSPARENT"),
	MKSIMPLESTYLE(0x4000, "TBSTYLE_", "REGISTERDROP"),
	MKSIMPLESTYLE(0x2000, "TBSTYLE_", "CUSTOMERASE"),
	MKSIMPLESTYLE(0x1000, "TBSTYLE_", "LIST"),
	MKSIMPLESTYLE(0x0800, "TBSTYLE_", "FLAT"),
	MKSIMPLESTYLE(0x0400, "TBSTYLE_", "ALTDRAG"),
	MKSIMPLESTYLE(0x0200, "TBSTYLE_", "WRAPABLE"),
	MKSIMPLESTYLE(0x0100, "TBSTYLE_", "TOOLTIPS"),
	MKCCSSIMPLESTYLEMAP()
SIMPLESTYLELUT_END();
SIMPLESTYLELUT_ENTRIES(g_ToolbarExStyles, 8)
	MKSIMPLESTYLE(0x00000200L, "TBSTYLE_EX_", "TOOLTIPSEXCLUDETOOLBAR"),
	MKSIMPLESTYLE(0x00000100L, "TBSTYLE_EX_", "TRANSPARENTDEADAREA"),
	MKSIMPLESTYLE(0x00000080L, "TBSTYLE_EX_", "DOUBLEBUFFER"),
	MKSIMPLESTYLE(0x00000010L, "TBSTYLE_EX_", "HIDECLIPPEDBUTTONS"),
	MKSIMPLESTYLE(0x00000008L, "TBSTYLE_EX_", "MIXEDBUTTONS"),
	MKSIMPLESTYLE(0x00000004L, "TBSTYLE_EX_", "VERTICAL"),
	MKSIMPLESTYLE(0x00000002L, "TBSTYLE_EX_", "MULTICOLUMN"),
	MKSIMPLESTYLE(0x00000001L, "TBSTYLE_EX_", "DRAWDDARROWS")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_ComboExStyles, 4)
	MKSIMPLESTYLE(0x0010, "CBS_", "OWNERDRAWFIXED"),
	MKSIMPLESTYLE(0x0003, "CBS_", "DROPDOWNLIST"),
	MKSIMPLESTYLE(0x0002, "CBS_", "DROPDOWN"),
	MKSIMPLESTYLE(0x0001, "CBS_", "SIMPLE")
SIMPLESTYLELUT_END();
SIMPLESTYLELUT_ENTRIES(g_ComboExExStyles, 6)
	MKSIMPLESTYLE(0x00000020L, "CBES_EX_", "TEXTENDELLIPSIS"),
	MKSIMPLESTYLE(0x00000010L, "CBES_EX_", "CASESENSITIVE"),
	MKSIMPLESTYLE(0x00000008L, "CBES_EX_", "NOSIZELIMIT"),
	MKSIMPLESTYLE(0x00000004L, "CBES_EX_", "PATHWORDBREAKPROC"),
	MKSIMPLESTYLE(0x00000002L, "CBES_EX_", "NOEDITIMAGEINDENT"),
	MKSIMPLESTYLE(0x00000001L, "CBES_EX_", "NOEDITIMAGE")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_RichEditStyles, 18)
	MKSIMPLESTYLE(0x01000000, "ES_", "SELECTIONBAR"),
	MKSIMPLESTYLE(0x00400000, "ES_", "VERTICAL"),
	MKSIMPLESTYLE(0x00040000, "ES_", "SELFIME"),
	MKSIMPLESTYLE(0x00080000, "ES_", "NOIME"),
	MKSIMPLESTYLE(0x00008000, "ES_", "SAVESEL"),
	MKSIMPLESTYLE(0x00004000, "ES_", "SUNKEN"),
	MKSIMPLESTYLE(0x00002000, "ES_", "DISABLENOSCROLL"), // ES_NUMBER?
	MKSIMPLESTYLE(0x00001000, "ES_", "WANTRETURN"),
	MKSIMPLESTYLE(0x00000800, "ES_", "READONLY"),
	MKSIMPLESTYLE(0x00000100, "ES_", "NOHIDESEL"),
	MKSIMPLESTYLE(0x00000080, "ES_", "AUTOHSCROLL"),
	MKSIMPLESTYLE(0x00000040, "ES_", "AUTOVSCROLL"),
	MKSIMPLESTYLE(0x00000020, "ES_", "PASSWORD"),
	MKSIMPLESTYLE(0x00000008, "ES_", "NOOLEDRAGDROP"),
	MKSIMPLESTYLE(0x00000004, "ES_", "MULTILINE"),
	MKSIMPLESTYLE(0x00000002, "ES_", "RIGHT"),
	MKSIMPLESTYLE(0x00000001, "ES_", "CENTER"),
	MKSIMPLESTYLE(0x00000000, "ES_", "LEFT")
SIMPLESTYLELUT_END();
SIMPLESTYLELUT_ENTRIES(g_RichEditSesStyles, 30)
	MKSIMPLESTYLE(0x20000000L, "SES_", "NOEALINEHEIGHTADJUST"),
	MKSIMPLESTYLE(0x10000000L, "SES_", "CTFNOLOCK"),
	MKSIMPLESTYLE(0x08000000L, "SES_", "MULTISELECT"),
	MKSIMPLESTYLE(0x04000000L, "SES_", "SMARTDRAGDROP"),
	MKSIMPLESTYLE(0x02000000L, "SES_", "WORDDRAGDROP"),
	MKSIMPLESTYLE(0x01000000L, "SES_", "LOGICALCARET"),
	MKSIMPLESTYLE(0x00800000L, "SES_", "CTFALLOWPROOFING"),
	MKSIMPLESTYLE(0x00400000L, "SES_", "CTFALLOWSMARTTAG"),
	MKSIMPLESTYLE(0x00200000L, "SES_", "CTFALLOWEMBED"),
	MKSIMPLESTYLE(0x00100000L, "SES_", "LBSCROLLNOTIFY"),
	MKSIMPLESTYLE(0x00080000L, "SES_", "CUSTOMLOOK"),
	MKSIMPLESTYLE(0x00040000L, "SES_", "USEATFONT"),
	MKSIMPLESTYLE(0x00020000L, "SES_", "HIDEGRIDLINES"),
	MKSIMPLESTYLE(0x00010000L, "SES_", "USECTF"),
	MKSIMPLESTYLE(0x00008000L, "SES_", "DRAFTMODE"),
	MKSIMPLESTYLE(0x00004000L, "SES_", "XLTCRCRLFTOCR"),
	MKSIMPLESTYLE(0x00002000L, "SES_", "SCROLLONKILLFOCUS"),
	MKSIMPLESTYLE(0x00001000L, "SES_", "BIDI"),
	MKSIMPLESTYLE(0x00000800L, "SES_", "NOINPUTSEQUENCECHK"),
	MKSIMPLESTYLE(0x00000400L, "SES_", "LOWERCASE"),
	MKSIMPLESTYLE(0x00000200L, "SES_", "UPPERCASE"),
	MKSIMPLESTYLE(0x00000100L, "SES_", "ALLOWBEEPS"),
	MKSIMPLESTYLE(0x00000080L, "SES_", "NOIME"),
	MKSIMPLESTYLE(0x00000040L, "SES_", "USEAIMM"),
	MKSIMPLESTYLE(0x00000020L, "SES_", "NOFOCUSLINKNOTIFY"),
	MKSIMPLESTYLE(0x00000010L, "SES_", "DEFAULTLATINLIGA"),
	MKSIMPLESTYLE(0x00000008L, "SES_", "HYPERLINKTOOLTIPS"),
	MKSIMPLESTYLE(0x00000004L, "SES_", "EXTENDBACKCOLOR"),
	MKSIMPLESTYLE(0x00000002L, "SES_", "BEEPONMAXTEXT"),
	MKSIMPLESTYLE(0x00000001L, "SES_", "EMULATESYSEDIT")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_SyslinkStyles, 6)
	MKSIMPLESTYLE(0x00000020L, "LWS_", "RIGHT"),
	MKSIMPLESTYLE(0x00000010L, "LWS_", "USECUSTOMTEXT"),
	MKSIMPLESTYLE(0x00000008L, "LWS_", "USEVISUALSTYLE"),
	MKSIMPLESTYLE(0x00000004L, "LWS_", "NOPREFIX"),
	MKSIMPLESTYLE(0x00000002L, "LWS_", "IGNORERETURN"),
	MKSIMPLESTYLE(0x00000001L, "LWS_", "TRANSPARENT")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_RebarStyles, 8+12)
	MKSIMPLESTYLE(0x8000, "RBS_", "DBLCLKTOGGLE"),
	MKSIMPLESTYLE(0x4000, "RBS_", "VERTICALGRIPPER"),
	MKSIMPLESTYLE(0x2000, "RBS_", "AUTOSIZE"),
	MKSIMPLESTYLE(0x1000, "RBS_", "REGISTERDROP"),
	MKSIMPLESTYLE(0x0800, "RBS_", "FIXEDORDER"),
	MKSIMPLESTYLE(0x0400, "RBS_", "BANDBORDERS"),
	MKSIMPLESTYLE(0x0200, "RBS_", "VARHEIGHT"),
	MKSIMPLESTYLE(0x0100, "RBS_", "TOOLTIPS"),
	MKCCSSIMPLESTYLEMAP()
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_PagerStyles, 4)
	MKSIMPLESTYLE(0x00000004L, "PGS_", "DRAGNDROP"),
	MKSIMPLESTYLE(0x00000002L, "PGS_", "AUTOSCROLL"),
	MKSIMPLESTYLE(0x00000001L, "PGS_", "HORZ"),
	MKSIMPLESTYLE(0x00000000L, "PGS_", "VERT")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_MonthCalStyles, 8)
	MKSIMPLESTYLE(0x0100, "MCS_", "NOSELCHANGEONNAV"),
	MKSIMPLESTYLE(0x0080, "MCS_", "SHORTDAYSOFWEEK"),
	MKSIMPLESTYLE(0x0040, "MCS_", "NOTRAILINGDATES"),
	MKSIMPLESTYLE(0x0010, "MCS_", "NOTODAY"),
	MKSIMPLESTYLE(0x0008, "MCS_", "NOTODAYCIRCLE"),
	MKSIMPLESTYLE(0x0004, "MCS_", "WEEKNUMBERS"),
	MKSIMPLESTYLE(0x0002, "MCS_", "MULTISELECT"),
	MKSIMPLESTYLE(0x0001, "MCS_", "DAYSTATE")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_DateTimeStyles, 7)
	MKSIMPLESTYLE(0x0040, "DTS_", "RIGHTALIGN"),
	MKSIMPLESTYLE(0x0010, "DTS_", "APPCANPARSE"),
	MKSIMPLESTYLE(0x000c, "DTS_", "SHORTDATECENTURYFORMAT"),
	MKSIMPLESTYLE(0x0009, "DTS_", "TIMEFORMAT"),
	MKSIMPLESTYLE(0x0004, "DTS_", "LONGDATEFORMAT"),
	MKSIMPLESTYLE(0x0002, "DTS_", "SHOWNONE"),
	MKSIMPLESTYLE(0x0001, "DTS_", "UPDOWN")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_UpDownStyles, 9)
	MKSIMPLESTYLE(0x0100, "UDS_", "HOTTRACK"),
	MKSIMPLESTYLE(0x0080, "UDS_", "NOTHOUSANDS"),
	MKSIMPLESTYLE(0x0040, "UDS_", "HORZ"),
	MKSIMPLESTYLE(0x0020, "UDS_", "ARROWKEYS"),
	MKSIMPLESTYLE(0x0010, "UDS_", "AUTOBUDDY"),
	MKSIMPLESTYLE(0x0008, "UDS_", "ALIGNLEFT"),
	MKSIMPLESTYLE(0x0004, "UDS_", "ALIGNRIGHT"),
	MKSIMPLESTYLE(0x0002, "UDS_", "SETBUDDYINT"),
	MKSIMPLESTYLE(0x0001, "UDS_", "WRAP")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_TrackbarStyles, 13)
	MKSIMPLESTYLE(0x00001000, "TBS_", "TRANSPARENTBKGND"),
	MKSIMPLESTYLE(0x00000800, "TBS_", "NOTIFYBEFOREMOVE"),
	MKSIMPLESTYLE(0x00000400, "TBS_", "DOWNISLEFT"),
	MKSIMPLESTYLE(0x00000200, "TBS_", "REVERSED"),
	MKSIMPLESTYLE(0x00000100, "TBS_", "TOOLTIPS"),
	MKSIMPLESTYLE(0x00000080, "TBS_", "NOTHUMB"),
	MKSIMPLESTYLE(0x00000040, "TBS_", "FIXEDLENGTH"),
	MKSIMPLESTYLE(0x00000020, "TBS_", "ENABLESELRANGE"),
	MKSIMPLESTYLE(0x00000010, "TBS_", "NOTICKS"),
	MKSIMPLESTYLE(0x00000008, "TBS_", "BOTH"),
	MKSIMPLESTYLE(0x00000004, "TBS_", "TOP/LEFT"),
	MKSIMPLESTYLE(0x00000002, "TBS_", "VERT"),
	MKSIMPLESTYLE(0x00000001, "TBS_", "AUTOTICKS")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_TooltipsStyles, 7)
	MKSIMPLESTYLE(0x00000100, "TTS_", "USEVISUALSTYLE"),
	MKSIMPLESTYLE(0x00000080, "TTS_", "CLOSE"),
	MKSIMPLESTYLE(0x00000040, "TTS_", "BALLOON"),
	MKSIMPLESTYLE(0x00000020, "TTS_", "NOFADE"),
	MKSIMPLESTYLE(0x00000010, "TTS_", "NOANIMATE"),
	MKSIMPLESTYLE(0x00000002, "TTS_", "NOPREFIX"),
	MKSIMPLESTYLE(0x00000001, "TTS_", "ALWAYSTIP")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_HeaderStyles, 12)
	MKSIMPLESTYLE(0x00001000, "HDS_", "OVERFLOW"),
	MKSIMPLESTYLE(0x00000800, "HDS_", "NOSIZING"),
	MKSIMPLESTYLE(0x00000400, "HDS_", "CHECKBOXES"),
	MKSIMPLESTYLE(0x00000200, "HDS_", "FLAT"),
	MKSIMPLESTYLE(0x00000100, "HDS_", "FILTERBAR"),
	MKSIMPLESTYLE(0x00000080, "HDS_", "FULLDRAG"),
	MKSIMPLESTYLE(0x00000040, "HDS_", "DRAGDROP"),
	MKSIMPLESTYLE(0x00000008, "HDS_", "HIDDEN"),
	MKSIMPLESTYLE(0x00000004, "HDS_", "HOTTRACK"),
	MKSIMPLESTYLE(0x00000002, "HDS_", "BUTTONS"),
	MKSIMPLESTYLE(0x00000001, "HDS_", "VERT?"), // HDS_HORZ is 0 so this would make sense
	MKSIMPLESTYLE(0x00000000, "HDS_", "HORZ")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_StatusbarStyles, 2+12)
	MKSIMPLESTYLE(0x0200, "SBARS_", "TOOLTIPS"),
	MKSIMPLESTYLE(0x0100, "SBARS_", "SIZEGRIP"),
	MKCCSSIMPLESTYLEMAP()
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_AnimateStyles, 4)
	MKSIMPLESTYLE(0x00000008, "ACS_", "TIMER"),
	MKSIMPLESTYLE(0x00000004, "ACS_", "AUTOPLAY"),
	MKSIMPLESTYLE(0x00000002, "ACS_", "TRANSPARENT"),
	MKSIMPLESTYLE(0x00000001, "ACS_", "CENTER")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_ProgressbarStyles, 4)
	MKSIMPLESTYLE(0x00000010, "PBS_", "SMOOTHREVERSE"),
	MKSIMPLESTYLE(0x00000008, "PBS_", "MARQUEE"),
	MKSIMPLESTYLE(0x00000004, "PBS_", "VERTICAL"),
	MKSIMPLESTYLE(0x00000001, "PBS_", "SMOOTH")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_ComboStyles, 13)
	MKSIMPLESTYLE(0x00004000, "CBS_", "LOWERCASE"),
	MKSIMPLESTYLE(0x00002000, "CBS_", "UPPERCASE"),
	MKSIMPLESTYLE(0x00000800, "CBS_", "DISABLENOSCROLL"),
	MKSIMPLESTYLE(0x00000400, "CBS_", "NOINTEGRALHEIGHT"),
	MKSIMPLESTYLE(0x00000200, "CBS_", "HASSTRINGS"),
	MKSIMPLESTYLE(0x00000100, "CBS_", "SORT"),
	MKSIMPLESTYLE(0x00000080, "CBS_", "OEMCONVERT"),
	MKSIMPLESTYLE(0x00000040, "CBS_", "AUTOHSCROLL"),
	MKSIMPLESTYLE(0x00000020, "CBS_", "OWNERDRAWVARIABLE"),
	MKSIMPLESTYLE(0x00000010, "CBS_", "OWNERDRAWFIXED"),
	MKSIMPLESTYLE(0x00000004, "CBS_", "DROPDOWNLIST"),
	MKSIMPLESTYLE(0x00000002, "CBS_", "DROPDOWN"),
	MKSIMPLESTYLE(0x00000001, "CBS_", "SIMPLE")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_ListboxStyles, 16)
	MKSIMPLESTYLE(0x00008000, "LBS_", "COMBOBOX"),
	MKSIMPLESTYLE(0x00004000, "LBS_", "NOSEL"),
	MKSIMPLESTYLE(0x00002000, "LBS_", "NODATA"),
	MKSIMPLESTYLE(0x00001000, "LBS_", "DISABLENOSCROLL"),
	MKSIMPLESTYLE(0x00000800, "LBS_", "EXTENDEDSEL"),
	MKSIMPLESTYLE(0x00000400, "LBS_", "WANTKEYBOARDINPUT"),
	MKSIMPLESTYLE(0x00000200, "LBS_", "MULTICOLUMN"),
	MKSIMPLESTYLE(0x00000100, "LBS_", "NOINTEGRALHEIGHT"),
	MKSIMPLESTYLE(0x00000080, "LBS_", "USETABSTOPS"),
	MKSIMPLESTYLE(0x00000040, "LBS_", "HASSTRINGS"),
	MKSIMPLESTYLE(0x00000020, "LBS_", "OWNERDRAWVARIABLE"),
	MKSIMPLESTYLE(0x00000010, "LBS_", "OWNERDRAWFIXED"),
	MKSIMPLESTYLE(0x00000008, "LBS_", "MULTIPLESEL"),
	MKSIMPLESTYLE(0x00000004, "LBS_", "NOREDRAW"),
	MKSIMPLESTYLE(0x00000002, "LBS_", "SORT"),
	MKSIMPLESTYLE(0x00000001, "LBS_", "NOTIFY")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_EditStyles, 14)
	MKSIMPLESTYLE(0x00002000, "ES_", "NUMBER"),
	MKSIMPLESTYLE(0x00001000, "ES_", "WANTRETURN"),
	MKSIMPLESTYLE(0x00000800, "ES_", "READONLY"),
	MKSIMPLESTYLE(0x00000400, "ES_", "OEMCONVERT"),
	MKSIMPLESTYLE(0x00000100, "ES_", "NOHIDESEL"),
	MKSIMPLESTYLE(0x00000080, "ES_", "AUTOHSCROLL"),
	MKSIMPLESTYLE(0x00000040, "ES_", "AUTOVSCROLL"),
	MKSIMPLESTYLE(0x00000020, "ES_", "PASSWORD"),
	MKSIMPLESTYLE(0x00000010, "ES_", "LOWERCASE"),
	MKSIMPLESTYLE(0x00000008, "ES_", "UPPERCASE"),
	MKSIMPLESTYLE(0x00000004, "ES_", "MULTILINE"),
	MKSIMPLESTYLE(0x00000002, "ES_", "RIGHT"),
	MKSIMPLESTYLE(0x00000001, "ES_", "CENTER"),
	MKSIMPLESTYLE(0x00000000, "ES_", "LEFT")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_ButtonStyles, 25)
	MKSIMPLESTYLE(0x00008000, "BS_", "FLAT"),
	MKSIMPLESTYLE(0x00004000, "BS_", "NOTIFY"),
	MKSIMPLESTYLE(0x00002000, "BS_", "MULTILINE"),
	MKSIMPLESTYLE(0x00001000, "BS_", "PUSHLIKE"),
	MKSIMPLESTYLE(0x00000C00, "BS_", "VCENTER"),
	MKSIMPLESTYLE(0x00000800, "BS_", "BOTTOM"),
	MKSIMPLESTYLE(0x00000400, "BS_", "TOP"),
	MKSIMPLESTYLE(0x00000300, "BS_", "CENTER"),
	MKSIMPLESTYLE(0x00000200, "BS_", "RIGHT"),
	MKSIMPLESTYLE(0x00000100, "BS_", "LEFT"),
	MKSIMPLESTYLE(0x00000080, "BS_", "BITMAP"),
	MKSIMPLESTYLE(0x00000040, "BS_", "ICON"),
	MKSIMPLESTYLE(0x00000020, "BS_", "LEFTTEXT/RIGHTBUTTON"),
	MKSIMPLESTYLE(0x0000000B, "BS_", "OWNERDRAW"), // BS_TYPEMASK
	MKSIMPLESTYLE(0x0000000A, "BS_", "PUSHBOX"),
	MKSIMPLESTYLE(0x00000009, "BS_", "AUTORADIOBUTTON"),
	MKSIMPLESTYLE(0x00000008, "BS_", "USERBUTTON"),
	MKSIMPLESTYLE(0x00000007, "BS_", "GROUPBOX"),
	MKSIMPLESTYLE(0x00000006, "BS_", "AUTO3STATE"),
	MKSIMPLESTYLE(0x00000005, "BS_", "3STATE"),
	MKSIMPLESTYLE(0x00000004, "BS_", "RADIOBUTTON"),
	MKSIMPLESTYLE(0x00000003, "BS_", "AUTOCHECKBOX"),
	MKSIMPLESTYLE(0x00000002, "BS_", "CHECKBOX"),
	MKSIMPLESTYLE(0x00000001, "BS_", "DEFPUSHBUTTON"),
	MKSIMPLESTYLE(0x00000000, "BS_", "PUSHBUTTON")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_StaticStyles, 30)
	MKSIMPLESTYLE(0x0000C000, "SS_", "WORDELLIPSIS"), // SS_ELLIPSISMASK
	MKSIMPLESTYLE(0x00008000, "SS_", "PATHELLIPSIS"),
	MKSIMPLESTYLE(0x00004000, "SS_", "ENDELLIPSIS"),
	MKSIMPLESTYLE(0x00002000, "SS_", "EDITCONTROL"),
	MKSIMPLESTYLE(0x00001000, "SS_", "SUNKEN"),
	MKSIMPLESTYLE(0x00000800, "SS_", "REALSIZEIMAGE"),
	MKSIMPLESTYLE(0x00000400, "SS_", "RIGHTJUST"),
	MKSIMPLESTYLE(0x00000200, "SS_", "CENTERIMAGE"),
	MKSIMPLESTYLE(0x00000100, "SS_", "NOTIFY"),
	MKSIMPLESTYLE(0x00000080, "SS_", "NOPREFIX"),
	MKSIMPLESTYLE(0x00000040, "SS_", "REALSIZECONTROL"),
	MKSIMPLESTYLE(0x00000012, "SS_", "ETCHEDFRAME"), // SS_TYPEMASK
	MKSIMPLESTYLE(0x00000011, "SS_", "ETCHEDVERT"),
	MKSIMPLESTYLE(0x00000010, "SS_", "ETCHEDHORZ"),
	MKSIMPLESTYLE(0x0000000F, "SS_", "ENHMETAFILE"),
	MKSIMPLESTYLE(0x0000000E, "SS_", "BITMAP"),
	MKSIMPLESTYLE(0x0000000D, "SS_", "OWNERDRAW"),
	MKSIMPLESTYLE(0x0000000C, "SS_", "LEFTNOWORDWRAP"),
	MKSIMPLESTYLE(0x0000000B, "SS_", "SIMPLE"),
	MKSIMPLESTYLE(0x0000000A, "SS_", "USERITEM"),
	MKSIMPLESTYLE(0x00000009, "SS_", "WHITEFRAME"),
	MKSIMPLESTYLE(0x00000008, "SS_", "GRAYFRAME"),
	MKSIMPLESTYLE(0x00000007, "SS_", "BLACKFRAME"),
	MKSIMPLESTYLE(0x00000006, "SS_", "WHITERECT"),
	MKSIMPLESTYLE(0x00000005, "SS_", "GRAYRECT"),
	MKSIMPLESTYLE(0x00000004, "SS_", "BLACKRECT"),
	MKSIMPLESTYLE(0x00000003, "SS_", "ICON"),
	MKSIMPLESTYLE(0x00000002, "SS_", "RIGHT"),
	MKSIMPLESTYLE(0x00000001, "SS_", "CENTER"),
	MKSIMPLESTYLE(0x00000000, "SS_", "LEFT")
SIMPLESTYLELUT_END();

SIMPLESTYLELUT_ENTRIES(g_ScrollbarStyles, 6)
	MKSIMPLESTYLE(0x00000010, "SBS_", "SIZEGRIP"),
	MKSIMPLESTYLE(0x00000008, "SBS_", "SIZEBOX"),
	MKSIMPLESTYLE(0x00000004, "SBS_", "BOTTOMALIGN/RIGHTALIGN"),
	MKSIMPLESTYLE(0x00000002, "SBS_", "TOPALIGN/LEFTALIGN"),
	MKSIMPLESTYLE(0x00000001, "SBS_", "VERT"),
	MKSIMPLESTYLE(0x00000000, "SBS_", "HORZ")
SIMPLESTYLELUT_END();

static const struct {
	UINT Msg;
	const SIMPLESTYLELUT*Styles;
} g_CtrlExStyle[] = {
	{ LVM_GETEXTENDEDLISTVIEWSTYLE, g_ListViewExStyles },
	{ TVM_GETEXTENDEDSTYLE, g_TreeViewExStyles },
	{ TCM_GETEXTENDEDSTYLE, g_TabExStyles },
	{ TB_GETEXTENDEDSTYLE, g_ToolbarExStyles },
	{ CBEM_GETEXTENDEDSTYLE, g_ComboExExStyles },
	{ EM_GETEDITSTYLE, g_RichEditSesStyles },
};

static const struct {
	LPCSTR Cls;
	const SIMPLESTYLELUT*Styles;
	LPCSTR FriendlyName;
} g_KnownControls[] = { // Note: The first types must match the order of g_CtrlExStyle!
	{ WC_LISTVIEWA, g_ListViewStyles, "ListView" },
	{ WC_TREEVIEWA, g_TreeViewStyles, "TreeView" },
	{ "SysTabControl32", g_TabStyles, "Tab" },
	{ TOOLBARCLASSNAMEA, g_ToolbarStyles, "Toolbar" },
	{ ("ComboBoxEx32"), g_ComboExStyles, "ComboBoxEx", },
	{ ("RichEdit"), g_RichEditStyles },
	{ ("#32770"), g_DialogStyles, "Dialog" },
	{ ("SysLink"), g_SyslinkStyles },
	{ ("ReBarWindow32"), g_RebarStyles, "ReBar" },
	{ ("SysPager"), g_PagerStyles },
	// ("SysIPAddress32"), no styles
	{ ("SysMonthCal32"), g_MonthCalStyles },
	{ ("SysDateTimePick32"), g_DateTimeStyles },
	{ ("msctls_updown32"), g_UpDownStyles, "UpDown" },
	{ ("msctls_trackbar32"), g_TrackbarStyles, "TrackBar" },
	{ ("tooltips_class32"), g_TooltipsStyles, "Tooltip" },
	{ ("SysHeader32"), g_HeaderStyles },
	{ STATUSCLASSNAMEA, g_StatusbarStyles, "StatusBar" },
	// HOTKEY_CLASS, no styles
	{ ("SysAnimate32"), g_AnimateStyles },
	{ ("msctls_progress32"), g_ProgressbarStyles, "ProgressBar" },
	{ ("ComboBox"), g_ComboStyles },
	{ ("ListBox"), g_ListboxStyles },
	{ ("Edit"), g_EditStyles },
	{ ("Button"), g_ButtonStyles },
	{ ("Static"), g_StaticStyles },
	{ ("ScrollBar"), g_ScrollbarStyles },
	{ ("ComboLBox"), g_ListboxStyles },
	// ("NativeFontCtl"), NFS_*?
	// ("msctls_netaddress"), ?
	// ("MDIClient"), ?
};

ATOM g_KnownAtoms[ARRAYSIZE(g_KnownControls)];

#ifdef DEBUG
static void VerifyStyle(LPCSTR Desc, const SIMPLESTYLELUT*Styles)
{
	UINT count = Styles[0].Bit, i, cch;
	for (i = 0;;)
	{
		if ((SIZE_T) Styles[++i].Name == (SIZE_T) DEBUG_SENTINEL)
		{
			if (count != i - 1)
			{
				TCHAR buf[42 * 10];
				cch = wsprintf(buf, TEXT("Expected count %u but got %u for "), count, i - 1);
				wsprintf(buf + cch, IS_INTRESOURCE(Desc) ? TEXT("%#x") : TEXT("%hs"), Desc);
				MessageBox(0, buf, 0, MB_ICONSTOP);
			}
			break;
		}
	}
}

static void VerifyStyles()
{
	for (UINT i = 0; i < ARRAYSIZE(g_KnownControls); ++i) VerifyStyle(g_KnownControls[i].Cls, g_KnownControls[i].Styles);
	for (UINT i = 0; i < ARRAYSIZE(g_CtrlExStyle); ++i) VerifyStyle(MAKEINTRESOURCEA(g_CtrlExStyle[i].Msg), g_CtrlExStyle[i].Styles);
}
#endif

static LPCSTR GetFriendlyClassName(ATOM Atom, LPCSTR Fallback)
{
	for (UINT i = 0; i < ARRAYSIZE(g_KnownControls); ++i)
	{
		if (g_KnownAtoms[i] == Atom)
		{
			if (g_KnownControls[i].FriendlyName)
				return g_KnownControls[i].FriendlyName;
			else if (Fallback == g_KnownControls[i].Cls && Fallback[0] == 'S' && Fallback[1] == 'y')
				return Fallback + 3; // Skip "Sys" prefix
			else
				break;
		}
	}
	return Fallback;
}

template<typename T> static LONG RW(const T&r) { return r.right - r.left; }
template<typename T> static LONG RH(const T&r) { return r.bottom - r.top; }

template <typename T, typename R> bool IsInRangeInclusive(T Val, R First, R Last)
{
	return Val >= (T) First && Val <= (T) Last;
}

template<typename T> void MemFree(T p)
{
	LocalFree((HLOCAL) p);
}

template<typename T> T MemAlloc(SIZE_T cb)
{
	return (T) LocalAlloc(LMEM_FIXED, cb);
}

template<typename T> static DECLSPEC_NOINLINE T GetProcAddr(LPCSTR Mod, LPCSTR Exp)
{
	return (T) GetProcAddress(LoadLibraryA(Mod), Exp);
}

template<typename T> static inline T GetProcAddr(LPCSTR Mod, LPCSTR Exp, T Fallback)
{
	T p = GetProcAddr<T>(Mod, Exp);
	return p ? p : Fallback;
}

template<typename D, typename S> static SIZE_T StrCpyRaw(D*Dst, const S*Src)
{
	for (SIZE_T i = 0;;++i)
		if (!(Dst[i] = (D) Src[i]))
			return i;
}

static inline bool PathIsAgnosticSeparator(UINT c)
{
	return c == '\\' || c == '/';
}

template<typename T> static T* PathFindLastComponent(const T*p)
{
	const T* s = 0;
	for (UINT i = 0; p[i]; ++i)
		if (PathIsAgnosticSeparator(p[i])) s = &p[i];
			return const_cast<T*>(s ? ++s : p);
}

template<typename T, typename M, typename D> static T Scale(T Value, M Mul, D Div)
{
#ifdef _WIN64
	return (T) (((INT64) Value * Mul) / Div);
#else
	return (T) MulDiv(Value, Mul, Div);
#endif
}

template<typename T> static T DpiScale(UINT Dpi, T Value)
{
	extern WORD g_SystemDpi;
	return Scale(Value, Dpi, g_SystemDpi);
}

static int WINAPI FallbackGetSystemMetricsForDpi(UINT SM, UINT Dpi)
{
	return DpiScale(Dpi, GetSystemMetrics(SM));
}

static BOOL WINAPI FallbackAdjustWindowRectExForDpi(LPRECT pR, DWORD Style, BOOL Menu, DWORD ExStyle, UINT Dpi)
{
	BOOL r = AdjustWindowRectEx(pR, Style, Menu, ExStyle);
	if (r)
	{
		pR->left = DpiScale(Dpi, pR->left);
		pR->top = DpiScale(Dpi, pR->top);
		pR->right = DpiScale(Dpi, pR->right);
		pR->bottom = DpiScale(Dpi, pR->bottom);
	}
	return r;
}

static HRESULT WINAPI FallbackGetDpiForMonitor(HMONITOR, UINT , UINT*DpiX, UINT*DpiY)
{
	extern WORD g_SystemDpi;
	return (*DpiX = *DpiY = g_SystemDpi, S_FALSE);
}

static UINT GetDpiForMonitor(HMONITOR hMon)
{
	extern WORD g_SystemDpi;
	UINT x, y;
	if (FAILED(g_GetDpiForMonitor(hMon, 0, &x, &y))) x = y = g_SystemDpi;
	return x;
}

static UINT WINAPI FallbackGetDpiForWindow(HWND hWnd)
{
	HMONITOR hMon = MonitorFromWindow(hWnd, MONITOR_DEFAULTTONEAREST);
	UINT dpi;
	HRESULT hr = g_GetDpiForMonitor(hMon, 0, &dpi, &dpi);
	return SUCCEEDED(hr) ? dpi : 0;
}

static HRESULT FallbackSetWindowTheme(HWND,LPCWSTR,LPCWSTR)
{
	return S_FALSE;
}

static HRESULT FallbackDwmGetWindowAttribute(HWND, DWORD, PVOID, DWORD)
{
	return HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
}

static HRESULT GetWindowVisibleRect(HWND hWnd, RECT*pR)
{
	HRESULT hr = g_DwmGetWindowAttribute(hWnd, DWMWA_EXTENDED_FRAME_BOUNDS, pR, sizeof(RECT));
	return SUCCEEDED(hr) ? hr : GetWindowRect(hWnd, pR) ? S_OK : E_FAIL;
}

template<typename T> static UINT MenuCheckedFlag(T Checked)
{
	return Checked ? MF_CHECKED : MF_UNCHECKED;
}

template<typename T> static UINT MenuEnabledFlag(T Enabled)
{
	return Enabled ? MF_ENABLED : (MF_DISABLED|MF_GRAYED);
}

template<UINT ItemBy = MF_BYCOMMAND> struct MenuItem {
	static UINT GetState(HMENU hMenu, UINT Id) { return GetMenuState(hMenu, Id, ItemBy); }
	static BOOL IsChecked(HMENU hMenu, UINT Id) { return GetState(hMenu, Id) & MF_CHECKED; }
};

static BOOL SetAlwaysOnTop(HWND hWnd, BOOL AoT, HWND hRequestor = HWND_TOP)
{
	UINT swp = SWP_ASYNCWINDOWPOS|SWP_NOMOVE|SWP_NOSIZE;
	if (!AoT) SetWindowPos(hWnd, hRequestor, 0, 0, 0, 0, swp|SWP_NOACTIVATE|SWP_NOSENDCHANGING|SWP_DEFERERASE);
	return SetWindowPos(hWnd, AoT ? HWND_TOPMOST : HWND_NOTOPMOST, 0, 0, 0, 0, swp|(AoT ? 0 : SWP_NOACTIVATE));
}

static inline UINT ModToVK(UINT Mod)
{
	// if (Mod == MOD_WIN) ...
	return Mod ? VK_SHIFT + ((Mod %= 4) ? Mod ^ 3 : 0) : 0;
}

static UINT GetVKNameText(UINT VK, LPTSTR Buffer, UINT Cap, BOOL NumPad = false)
{
	UINT sc = MapVirtualKey(LOBYTE(VK), MAPVK_VK_TO_VSC);
	if (!NumPad && IsInRangeInclusive(VK, VK_PRIOR, VK_DELETE) && VK != VK_SNAPSHOT) sc |= 0x100;
	return GetKeyNameText((sc << 16), Buffer, Cap);
}

static void GetAcceleratorTip(UINT Accel, LPWSTR Buffer)
{
	UINT mods = HIBYTE(Accel), key = LOBYTE(Accel), cch = 0, len, vkmod;
	*(UINT*) (Buffer+cch) = *(UINT*) TEXT(" ("), cch += 2;
	for (UINT i = 0; i < 3; ++i)
	{
		len = GetVKNameText(vkmod = ModToVK(mods & (1 << i)), Buffer+cch, 42);
		if (len && vkmod) cch += len, Buffer[cch++] = '+';
	}
	cch += GetVKNameText(key, Buffer+cch, 42);
	*(UINT*) (Buffer+cch) = *(UINT*) TEXT(")"), cch += 1;
}
