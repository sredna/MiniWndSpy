
/*****************************************************************************\
**                                                                           **
**  MiniWndSpy                                                               **
**                                                                           **
**  Licensed under the GNU General Public License v3.0 (the "License").      **
**  You may not use this file except in compliance with the License.         **
**  You can obtain a copy of the License at http://gnu.org/licenses/gpl-3.0  **
**                                                                           **
\*****************************************************************************/

#include "MiniWndSpy.h"
#include <stdlib.h>
#include <cstring>

#define CONFIG_SECTION TEXT("Config")

enum {
	IDC_LIST = 42, IDC_TBAR,
	IDC_MENU, IDC_BYMOUSE, IDC_BYTL, IDC_BYTR, IDC_BYBR, IDC_BYBL, IDC_LOCK, IDC_PARENT, IDC_CHILD, IDC_NEXT, IDC_PREV,
	IDC_AOT_NEVER, IDC_AOT_INACTIVE, IDC_AOT_ALWAYS,
	IDC_REFRESH, IDC_COPY, IDC_LOCKONKEY, IDC_ABOUT,
	IDC_EDITATTRIBUTE,
	IDC_FORCECLASS_NONE, IDC_FORCECLASS_FIRST, IDC_FORCECLASS_LAST = IDC_FORCECLASS_FIRST + ARRAYSIZE(g_KnownControls),
	TID_CAPTUREWINDOW = 1, TID_REFRESH, TID_STOPREFRESH,
	VER_A = 1, VER_B = 0, VER_C = ___BUILDDATE % 100000 / 10
};
enum {
	ROW_HWND = 0, ROW_CLASS, ROW_CAPTION, ROW_SEP1, ROW_SCLS, ROW_SWND, ROW_SEXT, ROW_SCTL, ROX_SCEX, ROW_PROP, 
	ROW_INFO, ROW_SEP2, ROW_PHWND, ROW_THWND, ROW_OHWND, ROW_SEP3, ROW_RW, ROW_RC, ROW_ID, ROW_HT,
	ROW_WDATA, ROW_DDATA, ROW_HINST, ROW_THREAD, ROW_SEP4, ROW_FHWND
};
LPCSTR g_RowNames[] = { "Handle", "Class", "Text", 0, "Class Style", "Window Style", "Extended Style", "Control Style", "Control ExStyle", "Properties", 
	"State", 0, "Parent", "Top-level", "Owner", 0, "Screen", "Client", "Control ID", "Hit-Test",
	"Window Data", "Dialog Data", "Instance", "Process:Thread", 0, "Foreground"
};

static const TCHAR g_AppName[] = TEXT("MiniWndSpy");
HWND g_hMain = 0, g_hList = 0, g_hTBar = 0, g_hFocus, g_hSelected = 0, g_hDesktop;
POINT g_HitPoint;
HHOOK g_hLockKbdHook = 0;
WINDOWINFO g_WindowInfo;
WORD g_PickerMode = 0, g_RichEditv1Atom = 0, g_Dpi = 0, g_SystemDpi;
SHORT g_ForceClass;
BYTE g_AOT = IDC_AOT_NEVER;
bool g_SysHasPerMonitorDpi, g_SysHasDWM = false;
const bool g_TBarVert = true;

static const struct { WORD Accel, Cmd; } g_AccelTable[] = {
	{ MAKEWORD(VK_OEM_MINUS, MOD_ALT), IDC_MENU },
	{ MAKEWORD(VK_F2, 0), IDC_EDITATTRIBUTE },
	{ MAKEWORD(VK_F5, 0), IDC_REFRESH },
	{ MAKEWORD('C', MOD_CONTROL), IDC_COPY },
	{ MAKEWORD('L', MOD_CONTROL), IDC_LOCK },
	{ MAKEWORD(VK_UP, MOD_CONTROL), IDC_PARENT },
	{ MAKEWORD(VK_DOWN, MOD_CONTROL), IDC_CHILD },
	{ MAKEWORD(VK_LEFT, MOD_CONTROL), IDC_PREV },
	{ MAKEWORD(VK_RIGHT, MOD_CONTROL), IDC_NEXT },
};

static inline bool IsLocked() { return g_PickerMode >= 0x8000; }
static inline WORD GetPickerType() { return g_PickerMode & ~0x8000; }

static LRESULT SendMessageTimeout(HWND hWnd, UINT Msg, WPARAM WPar, LPARAM LPar)
{
	LRESULT mr, r = SendMessageTimeout(hWnd, Msg, WPar, LPar, SMTO_ABORTIFHUNG, 500, &mr);
	return r ? mr : r;
}

static HWND GetAncestorParent(HWND hWnd) 
{
	HWND p = GetAncestor(hWnd, GA_PARENT);
	return p != g_hDesktop ? p : NULL;
}

static inline UINT GetMenuDropAlignment()
{
	return GetSystemMetrics(SM_MENUDROPALIGNMENT) ? TPM_RIGHTALIGN : TPM_LEFTALIGN;
}

static UINT ErrorDlg(HWND hOwner = NULL, UINT Error = GetLastError())
{
	TCHAR buf[MAX_PATH * 2];
	if (!Error) Error = ERROR_INTERNAL_ERROR;
	DWORD cch = FormatMessage(FORMAT_MESSAGE_IGNORE_INSERTS|FORMAT_MESSAGE_FROM_SYSTEM, 0, Error, 0, buf, ARRAYSIZE(buf), 0);
	MessageBox(hOwner, cch ? buf : TEXT("?"), 0, MB_OK|MB_ICONSTOP);
	return Error;
}

static UINT Die(HWND hWnd, UINT Error = GetLastError())
{
	ErrorDlg(hWnd, Error);
	PostMessage(hWnd, WM_QUIT, Error, Error); // Make sure we exit with this error code.
	SendMessage(hWnd, WM_CLOSE, Error, Error);
	return Error;
}

static void ClipboardCopy(LPCTSTR Str)
{
	HWND h = CreateWindowEx(WS_EX_TOOLWINDOW, WC_EDIT, Str, ES_MULTILINE, 0, 0, 0, 0, NULL, NULL, NULL, 0);
	SNDMSG(h, EM_SETSEL, 0, -1), SNDMSG(h, WM_CUT, 0, 0), SNDMSG(h, WM_CLOSE, 0, 0);
}

static INT_PTR HexStrToIntPtr(LPCTSTR Input)
{
	LPCWSTR fmt = sizeof(void*) > 4 ? L"%I64x" : L"%x";
	int s = 1 == swscanf(Input, fmt, &fmt);
	return s ? (INT_PTR) fmt : s;
}

static INT_PTR StrToIntPtrEx(LPCTSTR Input, LPTSTR*pEnd = NULL)
{
	LPCWSTR fmt = sizeof(void*) > 4 ? L"%I64i%n" : L"%i%n";
	int n = 0, s = 1 == swscanf(Input, fmt, &fmt, &n);
	if (pEnd) *pEnd = const_cast<LPTSTR>(Input) + (s ? n : s);
	return s ? (INT_PTR) fmt : s;
}

static INT_PTR StrToIntPtr(LPCTSTR Input, INT_PTR DefVal = 0)
{
	LPTSTR end;
	INT_PTR v = StrToIntPtrEx(Input, &end);
	return end > Input ? v : DefVal;
}

static HWND ParseStrToHwnd(LPCTSTR Input)
{
	UINT_PTR a = StrToIntPtr(Input), b = HexStrToIntPtr(Input);
	return (HWND) (a > b ? a : b);
}

static UINT GetConfigString(LPCTSTR File, LPCTSTR Section, LPCTSTR Name, PTSTR Buf, UINT Cap)
{
	UINT cch = GetPrivateProfileString(Section, Name, TEXT("\n"), Buf, Cap, File);
	return cch && *Buf != '\n' ? cch : *Buf = 0;
}

static int GetConfigInt(LPCTSTR File, LPCTSTR Section, LPCTSTR Name, int DefVal = 0)
{
	TCHAR buf[42];
	UINT len = GetConfigString(File, Section, Name, buf, ARRAYSIZE(buf));
	return len ? (int) StrToIntPtr(buf, DefVal) : DefVal;
}

static int GetConfigInt(LPCTSTR File, LPCTSTR Section, LPCTSTR Name, int DefVal, int MinVal, int MaxVal)
{
	int v = GetConfigInt(File, Section, Name, DefVal);
	return v < MinVal ? MinVal : v > MaxVal ? MaxVal : v;
}

static BOOL SetConfigInt(LPCTSTR File, LPCTSTR Section, LPCTSTR Name, int Val)
{
	TCHAR buf[42];
	wsprintf(buf, TEXT("%d"), Val);
	return WritePrivateProfileString(Section, Name, buf, File);
}

static UINT GetPortableConfigPath(PTSTR Buf, UINT Cap)
{
	UINT cch = GetModuleFileName(NULL, Buf, Cap), ext = 0, needDot = 0;
	for (UINT i = cch; i-- && !ext;)
		if (PathIsAgnosticSeparator(Buf[i]) || Buf[i] == '.')
			ext = Buf[i] == '.' ? i : needDot = cch;
	if (!ext || (cch = ext + 1 + 3) >= Cap) return 0;
	StrCpyRaw(Buf + ext + !needDot, ".ini" + !needDot);
	return cch;
}

static UINT GetConfigPath(PTSTR Buf, UINT Cap)
{
	UINT cch = GetPortableConfigPath(Buf, Cap), len;
	if (cch)
	{
		TCHAR buf[MAX_PATH];
		bool portable = GetConfigInt(Buf, CONFIG_SECTION, TEXT("Portable"), false) != false;
		if (!portable && SUCCEEDED(SHGetFolderPath(NULL, CSIDL_APPDATA|CSIDL_FLAG_CREATE, NULL, SHGFP_TYPE_CURRENT, buf)))
		{
			if ((len = lstrlen(buf)) + 1 + ARRAYSIZE(g_AppName) + 1 + 3 >= Cap || !len) return 0;
			return wsprintf(Buf, TEXT("%s\\%s%hs"), buf, g_AppName, ".ini");
		}
	}
	return cch;
}

static LRESULT CALLBACK LockKbdHookProc(int Code, WPARAM WPar, LPARAM LPar)
{
	KBDLLHOOKSTRUCT*pKHS = (KBDLLHOOKSTRUCT*) LPar;
	if (Code == 0 && WPar == WM_KEYUP)
		if (pKHS->vkCode == VK_CAPITAL)
			SendMessage(g_hMain, WM_COMMAND, IDC_LOCK, (SIZE_T) HWND_MESSAGE);
	return CallNextHookEx(g_hLockKbdHook, Code, WPar, LPar);
}

template<class T> static UINT StrCommaList(LPTSTR Out, UINT Pos, const T*Append)
{
	UINT cch = 0;
	if (Pos) cch += (UINT) StrCpyRaw(Out + Pos, ", ");
	return (UINT) StrCpyRaw(Out + Pos + cch, Append) + cch;
}

static int GetControlStyleKnownIndex()
{
	if (g_ForceClass < 0)
	{
		ATOM a = g_WindowInfo.cxWindowBorders ? (ATOM) g_WindowInfo.cxWindowBorders : g_WindowInfo.atomWindowType;
		for (UINT i = 0; i < ARRAYSIZE(g_KnownAtoms); ++i)
			if (g_KnownAtoms[i] == a)
				return i;
		return -1;
	}
	return g_ForceClass;
}

static const SIMPLESTYLELUT* GetControlStyleLut()
{
	int idx = GetControlStyleKnownIndex();
	return idx >= 0 ? g_KnownControls[idx].Styles : g_NoStyles;
}

template<class T> static UINT MapStyle(const T Lut[], UINT Bits, LPTSTR Out, UINT WindowStyle = 0, UINT Pos = 0)
{
	SimpleStyleLutHandlerInterface *pHandler = GetHandler(Lut);
	UINT cch = Pos, i, mapped = 0, count = pHandler->GetCount(Lut);
	for (Out[cch] = '\0', i = 0; ++i <= count;)
	{
		if (pHandler->Match(Lut[i], Bits, WindowStyle))
		{
			if (cch) cch += (UINT) StrCpyRaw(Out + cch, ", ");
			cch += (UINT) StrCpyRaw(Out + cch, pHandler->GetName(Lut[i], WindowStyle));
			Bits &= ~Lut[i].Bit, mapped |= Lut[i].Bit;
		}
	}
	if (Bits)
	{
		TCHAR buf[42];
		wsprintf(buf, TEXT("%X"), Bits);
		cch += StrCommaList(Out, cch, buf);
	}
	return mapped;
}

static int WINAPI GetRemoteWindowText(HWND hWnd, LPTSTR Buf, INT BufCap)
{
	DWORD_PTR r = SendMessageTimeout(hWnd, WM_GETTEXT, BufCap, (SIZE_T) Buf);
	return r ? (UINT) r : GetWindowText(hWnd, Buf, BufCap);
}

static void GetCombinedWindowInfo(HWND hWnd, LPTSTR Buf, UINT BufCap)
{
	UINT cch = 2 + wsprintf(Buf, PRI_HWND, hWnd), more;
	more = cch + 1 < BufCap ? GetClassName(hWnd, Buf + cch, BufCap - (cch + 1)) : 0;
	if (more) Buf[cch - 2] = ' ', Buf[cch - 1] = '[', *(UINT*)(&Buf[cch + more]) = ']', cch += more + 1;
	more = cch + 1 < BufCap ? GetRemoteWindowText(hWnd, Buf + cch + 1, BufCap - (cch + 1)) : 0;
	if (more) Buf[cch + 0] = ' ', cch += more + 1;
	if (cch + 1 >= BufCap && BufCap > 3) StrCpyRaw(&Buf[BufCap-4], "..."), cch = BufCap - 1;
}

static BOOL CALLBACK EnumPropsProc(HWND, LPTSTR Name, HANDLE Value, ULONG_PTR Cookie)
{
	TCHAR atombuf[42], valbuf[88], *Output = (LPTSTR) ((SIZE_T*)Cookie)[0];
	SIZE_T *cchAvail = &((SIZE_T*)Cookie)[1], *cchOffset = &((SIZE_T*)Cookie)[2], start;
	LPCTSTR fmt = sizeof(void*) > 4 ? TEXT("=0x%IX") : TEXT("=0x%X");
	UINT cchVal = wsprintf(valbuf, fmt, Value), cchName;
	if ((ATOM)(SIZE_T) Name == (SIZE_T) Name) 
		cchName = wsprintf(atombuf, TEXT("#%u"), (ATOM)(SIZE_T) Name), Name = atombuf;
	else
		cchName = lstrlen(Name);

	UINT first = !*cchOffset, cchReq = cchName + cchVal + (first ? 2 : 0);
	start = *cchOffset;
	if (start + cchReq < *cchAvail)
	{
		if (!first) start += StrCpyRaw(Output + start, TEXT(", "));
		start += StrCpyRaw(Output + start, Name);
		start += StrCpyRaw(Output + start, valbuf);
		*cchOffset = start;
	}
	return true;
}

static void UpdateSummary(HWND hWnd)
{
	DWORD_PTR mr;
	HWND hList = g_hList, hWndRel;
	TCHAR buf[MAX_PATH], smallbuf[42];
	LVITEM lvi;
	lvi.mask = LVIF_TEXT|LVIF_PARAM, lvi.iSubItem = 1;
	lvi.pszText = buf;
	RECT *pR, r;
	WINDOWINFO &wi = g_WindowInfo;
	if (!GetWindowInfo(hWnd, &wi)) wi.dwStyle = wi.dwExStyle = 0, wi.atomWindowType = 0;
	ATOM atom = g_ForceClass < 0 ? wi.atomWindowType : g_KnownAtoms[g_ForceClass];
	DWORD pid, tid = GetWindowThreadProcessId(hWnd, &pid), cch;

	wsprintf(buf, TEXT("%s - ") PRI_HWND, g_AppName, hWnd);
#ifdef DEBUG
	lstrcat(buf, TEXT(" (DEBUG)"));
#else
	if (pid == GetCurrentProcessId() && tid) hWnd = g_hSelected = 0; // Don't inspect ourself.
#endif
	SNDMSG(g_hMain, WM_SETTEXT, 0, (SIZE_T) (hWnd ? buf : g_AppName));

	SNDMSG(g_hTBar, TB_ENABLEBUTTON, IDC_CHILD, GetWindow(hWnd, GW_CHILD) != 0);
	SNDMSG(g_hTBar, TB_ENABLEBUTTON, IDC_NEXT, GetWindow(hWnd, GW_HWNDNEXT) != 0);
	SNDMSG(g_hTBar, TB_ENABLEBUTTON, IDC_PREV, GetWindow(hWnd, GW_HWNDPREV) != 0);

	GetCombinedWindowInfo(hWndRel = GetForegroundWindow(), buf, ARRAYSIZE(buf));
	if (!hWndRel) *buf = '\0';
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_FHWND, (SIZE_T)(&lvi));

	if (!hWnd)
	{
		lvi.lParam = 0, lvi.pszText = TEXT("");
		for (UINT i = 0; i < ARRAYSIZE(g_RowNames); ++i)
			if (i != ROW_FHWND)
				SNDMSG(hList, LVM_SETITEMTEXT, i, (SIZE_T)(&lvi));
		for (UINT i = IDC_PARENT; i <= IDC_PREV; ++i)
			SNDMSG(g_hTBar, TB_ENABLEBUTTON, i, false);
		return ;
	}

	UINT isHung = IsHungAppWindow(hWnd);
	wsprintf(buf, PRI_HWND, hWnd);
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_HWND, (SIZE_T)(&lvi));

	if ((cch = GetClassName(hWnd, buf, ARRAYSIZE(buf))) == 0) *((UINT*)buf) = '?';
	switch(wi.atomWindowType)
	{
	case 32768: StrCpyRaw(buf + cch, " (Menu)"); break;
	case 32769: StrCpyRaw(buf + cch, " (Desktop)"); break;
	case 32770: StrCpyRaw(buf + cch, " (Dialog)"); break;
	case 32771: StrCpyRaw(buf + cch, " (Switcher)"); break;
	case 32772: StrCpyRaw(buf + cch, " (Icon)"); break;
	}
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_CLASS, (SIZE_T)(&lvi));
	UINT classMagic = FASTCLASSHASH(buf), isRichEdit = 0;
	if (classMagic == FASTCLASSHASH(TEXT("RichEdit")))
	{
		isRichEdit = !lstrcmpi(TEXT("RichEdit50W"), buf) || !lstrcmpi(TEXT("RichEdit20W"), buf) || !lstrcmpi(TEXT("RichEdit20A"), buf);
		if (isRichEdit && g_RichEditv1Atom && g_ForceClass < 0) atom = g_RichEditv1Atom;
	}
	wi.cxWindowBorders = isRichEdit ? g_RichEditv1Atom : 0; // Store special ATOM here so we don't have to look it up again.

	(isHung ? GetWindowText : GetRemoteWindowText)(hWnd, buf, ARRAYSIZE(buf));
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_CAPTION, (SIZE_T)(&lvi));

	MapStyle(g_ClassStyles, (UINT) GetClassLongPtr(hWnd, GCL_STYLE), buf);
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_SCLS, (SIZE_T)(&lvi));

	// Must handle the control styles before the generic styles because RichEdit uses HIWORD styles!
	UINT scexmsg = 0, style_remain = wi.dwStyle;
	const SIMPLESTYLELUT*pscex = 0;
	UINT mask = (atom == g_RichEditv1Atom) ? 0xffffffff : 0x0000ffff, known = 0;
	for (UINT i = 0; !known && i < ARRAYSIZE(g_KnownControls); ++i)
	{
		if (atom == g_KnownAtoms[i])
		{
			if (i < ARRAYSIZE(g_CtrlExStyle) && !isHung)
			{
				scexmsg = g_CtrlExStyle[i].Msg;
				pscex = g_CtrlExStyle[i].Styles;
			}
			++known;
			style_remain &= ~MapStyle(g_KnownControls[i].Styles, wi.dwStyle & mask, buf, wi.dwStyle);
		}
	}
	if (style_remain) // Make sure we output something for unknown classes
	{
		UINT pos = known ? lstrlen(buf) : 0;
		MapStyle(g_NoStyles, style_remain & 0xffff, buf, 0, pos);
	}
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_SCTL, (SIZE_T)(&lvi));

	*buf = '\0';
	if (pscex) MapStyle(pscex, (UINT) SendMessageTimeout(hWnd, scexmsg, 0, 0), buf, wi.dwStyle);
	SNDMSG(hList, LVM_SETITEMTEXT, ROX_SCEX, (SIZE_T)(&lvi));

	MapStyle(g_WindowStyles, style_remain & 0xffff0000, buf, wi.dwStyle);
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_SWND, (SIZE_T)(&lvi));

	MapStyle(g_WindowExStyles, wi.dwExStyle, buf);
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_SEXT, (SIZE_T)(&lvi));

	SIZE_T propdata[3] = { (SIZE_T) buf, ARRAYSIZE(buf), *buf = '\0' };
	EnumPropsEx(hWnd, EnumPropsProc, (SIZE_T) propdata);
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_PROP, (SIZE_T)(&lvi));

	if (g_SysHasPerMonitorDpi)
		cch = wsprintf(buf, TEXT("%u DPI"), g_GetDpiForWindow(hWnd));
	else
		cch = *buf = 0;
	if (isHung) cch += StrCommaList(buf, cch, "Hung");
	if (IsWindowUnicode(hWnd)) cch += StrCommaList(buf, cch, "Unicode");
	if (!IsWindowEnabled(hWnd)) cch += StrCommaList(buf, cch, "Disabled");
	mr = isHung ? 0 : SendMessageTimeout(hWnd, WM_GETDLGCODE, 0, 0);
	if (mr) cch += StrCommaList(buf, cch, (wsprintf(smallbuf, TEXT("GDC=%#x"), (DWORD) mr), smallbuf));
	cch += StrCommaList(buf, cch, (wsprintf(smallbuf, TEXT("#%u"), wi.atomWindowType), smallbuf));
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_INFO, (SIZE_T)(&lvi));

	GetCombinedWindowInfo(hWndRel = GetAncestorParent(hWnd), buf, ARRAYSIZE(buf));
	if (!hWndRel) *buf = '\0';
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_PHWND, (SIZE_T)(&lvi));
	SNDMSG(g_hTBar, TB_ENABLEBUTTON, IDC_PARENT, *buf);

	GetCombinedWindowInfo(hWndRel = GetAncestor(hWnd, GA_ROOT), buf, ARRAYSIZE(buf));
	if (hWndRel == hWnd) *buf = '\0';
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_THWND, (SIZE_T)(&lvi));

	GetCombinedWindowInfo(hWndRel = GetWindow(hWnd, GW_OWNER), buf, ARRAYSIZE(buf));
	if (!hWndRel) *buf = '\0';
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_OHWND, (SIZE_T)(&lvi));

	mr = GetWindowLongPtr(hWnd, GWLP_USERDATA), *buf = 0;
	if (mr) wsprintf(buf, PRI_HWND, mr);
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_WDATA, (SIZE_T)(&lvi));

	mr = GetWindowLongPtr(hWnd, DWLP_USER), *buf = 0;
	if (mr) wsprintf(buf, PRI_HWND, mr);
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_DDATA, (SIZE_T)(&lvi));

	pR = &wi.rcWindow;
	cch = wsprintf(buf, TEXT("%dx%d..%dx%d (%dx%d)"), pR->left, pR->top, pR->right, pR->bottom, RW(*pR), RH(*pR));
	if (g_SysHasDWM && SUCCEEDED(g_DwmGetWindowAttribute(hWnd, DWMWA_EXTENDED_FRAME_BOUNDS, &r, sizeof(r))))
		if (!EqualRect(&r, pR))
			wsprintf(buf + cch, TEXT(" \x2243 %dx%d..%dx%d (%dx%d)"), r.left, r.top, r.right, r.bottom, RW(r), RH(r));
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_RW, (SIZE_T)(&lvi));

	pR = &wi.rcClient;
	wsprintf(buf, TEXT("%dx%d..%dx%d (%dx%d)"), pR->left, pR->top, pR->right, pR->bottom, RW(*pR), RH(*pR));
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_RC, (SIZE_T)(&lvi));

	mr = GetWindowLongPtr(hWnd, GWLP_ID);
	wsprintf(buf, sizeof(mr) > 4 ? TEXT("%Id (0x%IX)") : TEXT("%d (0x%X)"), mr, mr);
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_ID, (SIZE_T)(&lvi));

	mr = isHung ? HTHELP+42 : SendMessageTimeout(hWnd, WM_NCHITTEST, 0, MAKELONG(g_HitPoint.x, g_HitPoint.y));
	if ((INT) mr >= HTERROR && (INT) mr <= HTHELP)
		lvi.pszText = (LPTSTR) const_cast<LPCSTR>(g_HitTestStrings[mr + 2]);
	else
		lvi.pszText = TEXT("");
	SNDMSG(hList, LVM_SETITEMTEXTA, ROW_HT, (SIZE_T)(&lvi)), lvi.pszText = buf;

	mr = GetWindowLongPtr(hWnd, GWLP_HINSTANCE), *buf = 0;
	if (mr) wsprintf(buf, PRI_HWND, mr);
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_HINST, (SIZE_T)(&lvi));

	wsprintf(buf, tid ? TEXT("%u:%u") : TEXT(""), pid, tid);
	SNDMSG(hList, LVM_SETITEMTEXT, ROW_THREAD, (SIZE_T)(&lvi));
}

static BOOL CALLBACK ChildWindowFromPointProc(HWND hWnd, LPARAM Data)
{
	POINT &pt = *(POINT*) (((SIZE_T*)Data)[0]);
	SIZE_T &smallest = (((SIZE_T*)Data)[2]);
	HWND hExclude = (HWND) (((SIZE_T*)Data)[3]), hParent = (HWND) (((SIZE_T*)Data)[4]);
	RECT r;
	HRESULT hr = hExclude != hWnd ? GetWindowVisibleRect(hWnd, &r) : E_FAIL;
	if (PtInRect(&r, pt) && SUCCEEDED(hr))
	{
		if (hParent && !IsChild(hParent, hWnd)) return true; // We only care about siblings and children.
		SIZE_T area = RW(r) * RH(r);
		bool better = hParent ? area <= smallest : area < smallest; // Prefer child of same size
		if (better && IsWindowVisible(hWnd) && area)
		{
			((SIZE_T*)Data)[1] = (SIZE_T) hWnd; // Store new best match.
			if (hExclude) return false; // We only wanted a new top-level window.
			smallest = area;
			HWND hNewParent = GetAncestorParent(hWnd);
			if (hNewParent) ((SIZE_T*)Data)[4] = (SIZE_T) hNewParent; // Restrict search to siblings and children.
		}
	}
	return true;
}

static HWND ChildWindowFromPoint(const POINT&ScreenPt, HWND hExclude = NULL)
{
	// One would think RealChildWindowFromPoint could handle all edge cases but alas, it does not.
	// Instead of trying to handle BUTTON:BS_GROUPBOX and STATIC, we just look for the smallest child.
	HWND hWnd = WindowFromPoint(g_HitPoint);
	if (hWnd)
	{
restart:
		UINT mustRestart = false;
		HWND hParent = GetAncestorParent(hWnd);
		if (hParent) hWnd = hParent; // We need to search the parent of a BUTTON:BS_GROUPBOX.
		if (hExclude == hWnd) hWnd = 0, ++mustRestart; // EnumChildWindows will now act like EnumWindows.
		if (!mustRestart) hExclude = 0;
		SIZE_T data[] = { (SIZE_T) &ScreenPt, NULL, ~SIZE_T(0), (SIZE_T) hExclude, NULL };
		EnumChildWindows(hWnd, ChildWindowFromPointProc, (SIZE_T) data);
		if (data[1]) hWnd = (HWND) data[1];
		if (mustRestart && hWnd)
		{
			hExclude = 0;
			goto restart;
		}
	}
	return hWnd;
}

static inline void PickWindow(HWND hWnd, bool Lock = false)
{
	if (Lock) SendMessage(g_hMain, WM_COMMAND, IDC_LOCK, (SIZE_T) HWND_MESSAGE);
	g_ForceClass = -1;
	UpdateSummary(g_hSelected = hWnd);
}

static void PickWindowIfValid(HWND hWnd)
{
	if (hWnd) PickWindow(hWnd);
}

static void PickWindow()
{
	if (!IsLocked())
	{
		HWND hMain = g_hMain, hExclude = hMain;
		RECT r;
		GetWindowVisibleRect(hMain, &r);
		switch(GetPickerType())
		{
		case IDC_BYMOUSE: GetCursorPos(&g_HitPoint), hExclude = 0; break;
		case IDC_BYTL: g_HitPoint.x = r.left, g_HitPoint.y = r.top; break;
		case IDC_BYTR: g_HitPoint.x = r.right, g_HitPoint.y = r.top; break;
		case IDC_BYBR: g_HitPoint.x = r.right, g_HitPoint.y = r.bottom; break;
		case IDC_BYBL: g_HitPoint.x = r.left, g_HitPoint.y = r.bottom; break;
		}
		// Windows 10 has an invisible border area between GetWindowRect and DWMWA_EXTENDED_FRAME_BOUNDS.
		// We therefore have to ignore the transparent part of ourself during the window search.
		PickWindow(ChildWindowFromPoint(g_HitPoint, hExclude));
	}
}


static void AppendStyleMenu(HMENU hMenu, UINT FirstId, UINT Style, UINT Mask, const SIMPLESTYLELUT Lut[])
{
	SimpleStyleLutHandlerInterface *pHandler = GetHandler(Lut);
	UINT count = pHandler->GetCount(Lut), hideUnknown = GetKeyState(VK_SHIFT) >= 0;
	for (UINT any = 0, i = 0; i < 32; ++i)
	{
		UINT bit = 1 << i, known = false;
		LPCSTR name = "?";
		for (UINT j = 0; ++j <= count;)
			if (Lut[j].Bit == bit)
				name = pHandler->GetName(Lut[j], Style), known++;
		if (!known && (hideUnknown || !(bit & Mask))) continue;
		TCHAR buf[MAX_PATH];
		wsprintf(buf, L"%S\t%.8X", const_cast<LPCSTR>(name), bit);
		StrCpyRaw((LPSTR) buf, buf);
		if (!any) AppendMenuA(hMenu, MF_SEPARATOR, 0, 0), any++;
		AppendMenuA(hMenu, MF_STRING|MenuCheckedFlag(Style & bit), FirstId + i, (LPSTR) buf);
	}
}

static void DisplayAttributeMenu(LPARAM LPar)
{
	enum { IDC_WND_ENABLED = IDC_FORCECLASS_FIRST, IDC_WND_VISIBLE, IDC_WND_AOT, IDC_WND_CLOSE, IDC_STYLE_FIRST };
	HWND hList = g_hList, hWndAtt = 0, hParent = 0;
	const SIMPLESTYLELUT*pStyleLut = 0;
	TCHAR buf[MAX_PATH];
	LVITEM lvi;
	lvi.mask = LVIF_TEXT|LVIF_PARAM, lvi.iSubItem = 1;
	lvi.pszText = buf, lvi.cchTextMax = ARRAYSIZE(buf);
	lvi.iItem = ListView_GetNextItem(hList, -1, LVNI_SELECTED);
	UINT haveSelWnd = IsWindow(hWndAtt = g_hSelected);
	
	if (lvi.iItem != -1 && ListView_GetItem(hList, &lvi))
	{
		RECT r;
		int x = GET_X_LPARAM(LPar), y = GET_Y_LPARAM(LPar), knownidx = 0;
		ListView_GetItemRect(hList, lvi.iItem, &r, LVIR_LABEL);
		MapWindowPoints(hList, NULL, (POINT*) &r, 2);
		if ((UINT) LPar == (UINT) -1) x = r.left + (RW(r) / 2), y = r.top + (RH(r) / 2);
		UINT style = 0, exStyle = 0;

		HMENU hMenu = CreatePopupMenu();
		if (lvi.iItem == ROW_HWND || IsInRangeInclusive(lvi.iItem, ROW_PHWND, ROW_OHWND))
		{
			if ((hWndAtt = (HWND) HexStrToIntPtr(lvi.pszText)) != NULL)
			{
				style = (UINT) GetWindowLongPtr(hWndAtt, GWL_STYLE);
				exStyle = (UINT) GetWindowLongPtr(hWndAtt, GWL_EXSTYLE);
				AppendMenuA(hMenu, MF_STRING|MenuCheckedFlag(!(style & WS_DISABLED)), IDC_WND_ENABLED, "Enabled");
				AppendMenuA(hMenu, MF_STRING|MenuCheckedFlag(style & WS_VISIBLE), IDC_WND_VISIBLE, "Visible");
				hParent = GetAncestorParent(hWndAtt);
				if (!hParent) AppendMenuA(hMenu, MF_STRING|MenuCheckedFlag(exStyle & WS_EX_TOPMOST), IDC_WND_AOT, "Always on Top");
				AppendMenuA(hMenu, MF_SEPARATOR, 0, 0);
				AppendMenuA(hMenu, MF_STRING, IDC_WND_CLOSE, "Close");
				AppendMenuA(hMenu, MF_SEPARATOR, 0, 0);
			}
		}
		AppendMenuA(hMenu, MF_STRING|(*lvi.pszText ? 0 : MF_GRAYED), IDC_COPY, "&Copy" "\tCtrl+C");
		AppendMenuA(hMenu, MF_SEPARATOR, 0, 0);
		AppendMenuA(hMenu, MF_STRING, IDC_REFRESH, "&Refresh" "\tF5");
		switch(haveSelWnd ? lvi.iItem : ROW_SEP1)
		{
		case ROW_SWND:
			AppendStyleMenu(hMenu, IDC_STYLE_FIRST, g_WindowInfo.dwStyle, 0xffff0000, pStyleLut = g_WindowStyles);
			break;
		case ROW_SEXT:
			AppendStyleMenu(hMenu, IDC_STYLE_FIRST, g_WindowInfo.dwExStyle, ~UINT(0), pStyleLut = g_WindowExStyles);
			break;
		case ROW_SCTL:
			AppendStyleMenu(hMenu, IDC_STYLE_FIRST, g_WindowInfo.dwStyle, 0x0000ffff, pStyleLut = GetControlStyleLut());
			break;
		case ROX_SCEX:
			knownidx = GetControlStyleKnownIndex();
			if (knownidx >= 0 && knownidx < ARRAYSIZE(g_CtrlExStyle))
			{
				style = (UINT) SendMessageTimeout(hWndAtt, g_CtrlExStyle[knownidx].Msg, 0, 0);
				AppendStyleMenu(hMenu, IDC_STYLE_FIRST, style, ~UINT(0), pStyleLut = g_CtrlExStyle[knownidx].Styles);
			}
			break;
		}

		UINT cmd = TrackPopupMenu(hMenu, GetMenuDropAlignment()|TPM_VERTICAL|TPM_RETURNCMD, x, y, 0, hList, NULL);
		switch(cmd)
		{
		case IDC_COPY:
			switch(GetKeyState(VK_SHIFT) < 0 ? lvi.iItem : -1)
			{
			case ROW_SWND: wsprintf(lvi.pszText = buf, TEXT("0x%X"), g_WindowInfo.dwStyle); break;
			case ROW_SEXT: wsprintf(lvi.pszText = buf, TEXT("0x%X"), g_WindowInfo.dwExStyle); break;
			}
			ClipboardCopy(lvi.pszText);
			break;

		delayed_refresh:
			SetTimer(g_hMain, TID_REFRESH, 500, NULL); // ShowWindowAsync can be slow
			SetTimer(g_hMain, TID_STOPREFRESH, 2000+50, NULL);
		refresh:
		case IDC_REFRESH:
			PostMessage(g_hMain, WM_APPCOMMAND, cmd = 0, MAKELONG(0, APPCOMMAND_BROWSER_REFRESH));
			break;

		case IDC_WND_ENABLED:
			EnableWindow(hWndAtt, (style & WS_DISABLED));
			goto refresh;
		case IDC_WND_VISIBLE:
			ShowWindowAsync(hWndAtt, (style & WS_VISIBLE) ? SW_HIDE : SW_SHOW);
			goto delayed_refresh;
		case IDC_WND_AOT:
			SetAlwaysOnTop(hWndAtt, !(exStyle & WS_EX_TOPMOST), g_hMain);
			goto delayed_refresh;
		case IDC_WND_CLOSE:
			PostMessage(hWndAtt, WM_CLOSE, 0, 0);
			break;
		}
		if (cmd >= IDC_STYLE_FIRST && cmd < IDC_STYLE_FIRST + 32)
		{
			LONG_PTR orgbits, newbits;
			UINT set = !MenuItem<>::IsChecked(hMenu, cmd), bit = 1 << (cmd - IDC_STYLE_FIRST), msg;
			if (lvi.iItem == ROX_SCEX)
			{
				orgbits = style, newbits = (orgbits & ~bit) | (set ? bit : 0);
				msg = g_CtrlExStyle[knownidx].Msg;
				if (msg == EM_GETEDITSTYLE) newbits &= bit; // Message parameters are reversed!
				SendMessageTimeout(hWndAtt, msg - 1, bit, newbits); // Luckily all SET messages are GET-1
			}
			else
			{
				UINT gwl = lvi.iItem == ROW_SEXT ? GWL_EXSTYLE : GWL_STYLE;
				orgbits = GetWindowLongPtr(hWndAtt, gwl), newbits = (orgbits & ~bit) | (set ? bit : 0);
				SetWindowLongPtr(hWndAtt, gwl, newbits);
			}
			if (newbits != orgbits)
			{
				SetWindowPos(hWndAtt, 0, 0, 0, 0, 0, SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOMOVE|SWP_FRAMECHANGED|SWP_ASYNCWINDOWPOS);
				InvalidateRect(hWndAtt, NULL, false);
				goto refresh;
			}
		}

		DestroyMenu(hMenu);
	}
}

static void RefreshCachedDpiValues()
{
	HDC hDC = GetDC(NULL);
	g_SystemDpi = (WORD) GetDeviceCaps(hDC, LOGPIXELSY);
	ReleaseDC(NULL, hDC);
	g_Dpi = (WORD) g_GetDpiForWindow(g_hMain);
	if (!g_Dpi) g_Dpi = g_SystemDpi;
}

static void AdjustLastColumn()
{
	UINT w = (UINT) LVSCW_AUTOSIZE_USEHEADER;
	HMONITOR hMon = MonitorFromWindow(g_hList, MONITOR_DEFAULTTONULL);
	MONITORINFO mi;
	if (GetMonitorInfo(hMon, (mi.cbSize = sizeof(mi), &mi))) w = (RW(mi.rcWork) * 3) / 4; // As wide as possible while still leaving room for the first column and borders
	ListView_SetColumnWidth(g_hList, 1, w);
}

static void RefreshControlMetrics()
{
	RefreshCachedDpiValues();
	HINSTANCE hInst = HINST_APPLICATION;

	UINT imsiz = 15, imid = IDB_TOOLBAR16, tbv = g_TBarVert;
	if (g_Dpi >= (96 * 150 / 100)) imsiz *= 2, imid += 1; // 144dpi (150%)
	HIMAGELIST hIL = ImageList_LoadImageA(hInst, (LPSTR)(imid), imsiz, 0, CLR_DEFAULT, IMAGE_BITMAP, LR_CREATEDIBSECTION), hOldIL;
	SendMessage(g_hTBar, WM_SETTINGCHANGE, 0, 0);
	hOldIL = (HIMAGELIST) SendMessage(g_hTBar, TB_SETIMAGELIST, 0, (SIZE_T) hIL);
	if (hOldIL) ImageList_Destroy(hOldIL);
	TBBUTTON tbb;
	for (UINT i = 0; SendMessage(g_hTBar, TB_GETBUTTON, i, (SIZE_T) &tbb); ++i)
	{
		if ((tbb.fsStyle & BTNS_SEP) && tbv)
		{
			tbb.iBitmap =  g_GetSystemMetricsForDpi(SM_CYEDGE, g_Dpi) * 3;
			SendMessage(g_hTBar, TB_INSERTBUTTON, i, (SIZE_T) &tbb);
			SendMessage(g_hTBar, TB_DELETEBUTTON, i + 1, 0);
		}
	}

	SendMessage(g_hList, WM_SETTINGCHANGE, 0, 0);
	ListView_SetColumnWidth(g_hList, 0, LVSCW_AUTOSIZE), AdjustLastColumn();
}

static void InitializeMainWindow()
{
	RECT r;
	HWND hWnd = g_hMain;
	RefreshControlMetrics();
	
	HMONITOR hSrcMon = 0, hDstMon = 0;
	WINDOWPLACEMENT wp;
	GetWindowPlacement(hWnd, (wp.length = sizeof(wp), &wp));
	r.left = wp.rcNormalPosition.left, r.top = wp.rcNormalPosition.top;
	hDstMon = MonitorFromRect(&wp.rcNormalPosition, MONITOR_DEFAULTTONULL);
	wp.length = 0; // Make first SetWindowPlacement fail if there is no saved state.
	TCHAR ini[MAX_PATH], showCmd = SW_SHOWDEFAULT;
	if (GetConfigPath(ini, ARRAYSIZE(ini)))
	{
		g_AOT = BYTE(GetConfigInt(ini, CONFIG_SECTION, TEXT("AOT"), g_AOT - IDC_AOT_NEVER, 0, 2) + IDC_AOT_NEVER);
		g_PickerMode = WORD(GetConfigInt(ini, CONFIG_SECTION, TEXT("Pick"), 0, 0, IDC_BYBL - IDC_BYMOUSE) + IDC_BYMOUSE);

		GetPrivateProfileStruct(CONFIG_SECTION, TEXT("WP"), &wp, sizeof(wp), ini);
		hSrcMon = MonitorFromRect(&wp.rcNormalPosition, MONITOR_DEFAULTTONULL);
		showCmd = LOWORD(wp.showCmd), wp.showCmd = SW_HIDE;
		if (hSrcMon != hDstMon && hSrcMon)
		{
			UINT srcDpi = GetDpiForMonitor(hSrcMon), dstDpi = GetDpiForMonitor(hDstMon);
			wp.rcNormalPosition.right = r.left + Scale(RW(wp.rcNormalPosition), dstDpi, srcDpi);
			wp.rcNormalPosition.bottom = r.top + Scale(RH(wp.rcNormalPosition), dstDpi, srcDpi);
			wp.rcNormalPosition.left = r.left, wp.rcNormalPosition.top = r.top;
		}

		STARTUPINFO si;
		GetStartupInfo((si.cb = sizeof(si), &si));
		if (si.dwFlags & STARTF_USESHOWWINDOW) showCmd = si.wShowWindow;
	}

	if (SetWindowPlacement(hWnd, &wp))
	{
		ShowWindow(hWnd, showCmd == SW_SHOWMINIMIZED ? SW_SHOWNORMAL : showCmd); // Ignore minimized saved state.
	}
	else
	{
		GetWindowPlacement(hWnd, (wp.length = sizeof(wp), &wp));
		GetWindowInfo(hWnd, &g_WindowInfo);
		ListView_GetItemRect(g_hList, ARRAYSIZE(g_RowNames) - 1, &r, LVIR_BOUNDS), r.top = 0;
		MapWindowPoints(g_hList, NULL, (POINT*) &r, 2);
		UINT yhscroll = g_GetSystemMetricsForDpi(SM_CYHSCROLL, g_Dpi);
		UINT yedge = g_GetSystemMetricsForDpi(SM_CYEDGE, g_Dpi);
		r.right = 6 * (UINT) ListView_GetColumnWidth(g_hList, 0), r.left = 0;
		r.bottom = RH(r) + yhscroll + (yedge * 4), r.top = 0;
		g_AdjustWindowRectExForDpi(&r, g_WindowInfo.dwStyle, false, g_WindowInfo.dwExStyle, g_Dpi);
		wp.rcNormalPosition.right = wp.rcNormalPosition.left + RW(r);
		wp.rcNormalPosition.bottom = wp.rcNormalPosition.top + RH(r);
		wp.showCmd = showCmd;
		SetWindowPlacement(hWnd, &wp);
	}

	AdjustLastColumn();
	PostMessage(hWnd, WM_COMMAND, g_AOT, 0); // Set AOT.
	PostMessage(hWnd, WM_COMMAND, g_PickerMode, 0); // Activate picker.
}

static LRESULT CALLBACK MainWndProc(HWND hWnd, UINT Msg, WPARAM WPar, LPARAM LPar)
{
	HWND hCtl;
	NMHDR*pNMH = (NMHDR*) LPar;
	switch(Msg)
	{
	case WM_SIZE:
		{
			if (WPar == SIZE_MINIMIZED && !IsLocked())
			{
				SetTimer(hWnd, TID_CAPTUREWINDOW, g_PickerMode <= IDC_BYMOUSE ? 3000 : 3000 * 8, NULL); // Slow down the timer when minimized.
			}
			UINT x = 0, y = 0, w = LOWORD(LPar), h = HIWORD(LPar), tbv = g_TBarVert;
			RECT r;
			SendMessage(g_hTBar, TB_GETITEMRECT, 0, (SIZE_T) &r);
			if (tbv) x += RW(r); else y += RH(r);
			SetWindowPos(g_hTBar, NULL, 0, 0, tbv ? x : w, tbv ? h : y, SWP_NOZORDER|SWP_NOACTIVATE);
			SetWindowPos(g_hList, NULL, x, y, w - x, h - y, SWP_NOZORDER|SWP_NOACTIVATE);
		}
		break;

	case WM_GETMINMAXINFO:
		((MINMAXINFO*)LPar)->ptMinTrackSize.y |= 100;
		break;

	case WM_MOVING:
		if (!IsLocked() && GetPickerType() >= IDC_BYTL) PickWindow();
		break;

	case WM_ENTERSIZEMOVE:
		ListView_CancelEditLabel(g_hList);
		break;

	case WM_EXITSIZEMOVE:
		AdjustLastColumn();
		break;

	case WM_TIMER:
		if (WPar == TID_CAPTUREWINDOW) PickWindow();
		if (WPar == TID_REFRESH) SendMessage(hWnd, WM_COMMAND, IDC_REFRESH, 0);
		if (WPar == TID_STOPREFRESH) KillTimer(hWnd, IDC_REFRESH), KillTimer(hWnd, WPar);
		break;

	case WM_NOTIFY:
		switch((WPar << 16) ^ pNMH->code)
		{
		case (IDC_TBAR << 16) ^ TBN_GETINFOTIP:
			{
				LPNMTBGETINFOTIP ptbgit = (LPNMTBGETINFOTIP) pNMH;
				SIZE_T cch = 0;
				if (ptbgit->lParam && ptbgit->cchTextMax >= 200)
				{
					for (UINT i = 0; i < ARRAYSIZE(g_AccelTable); ++i)
					{
						if (g_AccelTable[i].Cmd == ptbgit->iItem)
						{
							cch = StrCpyRaw(ptbgit->pszText, (LPTSTR) ptbgit->lParam);
							GetAcceleratorTip(g_AccelTable[i].Accel, ptbgit->pszText + cch);
						}
					}
				}
				if (!cch) ptbgit->pszText = (LPTSTR) ptbgit->lParam;
			}
			break;
		
		case (IDC_LIST << 16) ^ LVN_ITEMCHANGED:
			{
				NMLISTVIEW&nmlv = *(NMLISTVIEW*) pNMH;
				if ((nmlv.uChanged & LVIF_STATE) && !(nmlv.uNewState & LVIS_SELECTED))
				{
					ListView_SetItemState(pNMH->hwndFrom, nmlv.iItem, 0, LVIS_FOCUSED); // Remove the ugly focus rectangle if there is no selection
				}
			}
			break;

		case (IDC_LIST << 16) ^ LVN_BEGINLABELEDIT:
			if (g_hSelected)
			{
				NMLVDISPINFO &nmlvdi = *(NMLVDISPINFO*) pNMH;
				TCHAR buf[MAX_PATH];
				LVITEM lvi;
				lvi.iSubItem = 1;
				lvi.pszText = buf, lvi.cchTextMax = ARRAYSIZE(buf);
				HWND hEdit = ListView_GetEditControl(pNMH->hwndFrom);
				if (nmlvdi.item.iItem == ROW_HWND || nmlvdi.item.iItem == ROW_CAPTION)
				{
					SendMessage(pNMH->hwndFrom, LVM_GETITEMTEXT, nmlvdi.item.iItem, (SIZE_T) &lvi);
					SendMessage(hEdit, WM_SETTEXT, 0, (SIZE_T) lvi.pszText);
					PostMessage(hWnd, WM_COMMAND, MAKELONG(0, EN_UPDATE), (SIZE_T) hEdit);
					return S_OK;
				}
			}
			return S_FALSE;

		case (IDC_LIST << 16) ^ LVN_ENDLABELEDIT:
			{
				NMLVDISPINFO &nmlvdi = *(NMLVDISPINFO*) pNMH;
				if (nmlvdi.item.iItem == ROW_HWND && nmlvdi.item.pszText)
					if (IsWindow(hCtl = ParseStrToHwnd(nmlvdi.item.pszText)))
						PickWindow(hCtl, true);
					else
						MessageBeep(MB_ICONSTOP);

				if (nmlvdi.item.iItem == ROW_CAPTION && nmlvdi.item.pszText)
					if ((int) SendMessageTimeout(g_hSelected, WM_SETTEXT, 0, (SIZE_T) nmlvdi.item.pszText) > 0)
						ListView_SetItemText(pNMH->hwndFrom, nmlvdi.item.iItem, 1, nmlvdi.item.pszText);
			}
			return false;
		}
		break;

	case WM_COMMAND:
		if (WPar >= IDC_BYMOUSE && WPar <= IDC_BYBL) doSetPickerMode:
		{
			for (UINT i = IDC_BYMOUSE; i <= IDC_BYBL; ++i)
			{
				SNDMSG(g_hTBar, TB_CHECKBUTTON, i, WPar == (WPARAM) i);
				if (WPar == (WPARAM) i)
				{
					SNDMSG(g_hTBar, TB_CHECKBUTTON, IDC_LOCK, false);
					g_PickerMode = (WORD) WPar;
					PickWindow();
					SetTimer(hWnd, TID_CAPTUREWINDOW, WPar <= IDC_BYMOUSE ? 500 : 1000, NULL);
				}
			}
		}

		if (WPar >= IDC_FORCECLASS_FIRST && WPar <= IDC_FORCECLASS_LAST)
		{
			SendMessage(hWnd, WM_COMMAND, IDC_LOCK, (SIZE_T) HWND_MESSAGE);
			g_ForceClass = LOWORD(WPar - IDC_FORCECLASS_FIRST);
			UpdateSummary(g_hSelected);
		}

		if (HIWORD(WPar) == EN_UPDATE && (HWND) LPar == ListView_GetEditControl(g_hList))
		{
			RECT r;
			GetClientRect(g_hList, &r);
			UINT w = r.right, h;
			GetWindowRect((HWND) LPar, &r), h = RH(r);
			MapWindowPoints(NULL, g_hList, (POINT*) &r, 1);
			SetWindowPos((HWND) LPar, 0, 0, r.top, w, h, SWP_NOZORDER|SWP_NOACTIVATE);
		}

		switch(LOWORD(WPar))
		{
		case IDC_MENU:
			{
				INT_PTR idx = SendMessage((HWND) (LPar = (SIZE_T) g_hTBar), TB_COMMANDTOINDEX, IDC_MENU, 0);
				UINT tpm_align = GetMenuDropAlignment(), ralign = tpm_align != TPM_LEFTALIGN, mf;
				RECT r;
				SendMessage((HWND) LPar, TB_GETITEMRECT, idx, (SIZE_T) &r);
				MapWindowPoints((HWND) LPar, NULL, (POINT*) &r, 2);
				r.left = ralign ? r.right : r.left, r.top = r.bottom;
				HMENU hMenu = CreatePopupMenu(), hSubMenu;
				mf = g_PickerMode <= IDC_BYMOUSE ? MF_GRAYED : 0;
				AppendMenuA(hMenu, MF_POPUP|mf, (SIZE_T) (hSubMenu = CreatePopupMenu()), "Interpret as");
				AppendMenuA(hSubMenu, MF_STRING|MenuCheckedFlag(g_ForceClass < 0), IDC_FORCECLASS_NONE, "Auto");
				AppendMenuA(hSubMenu, MF_SEPARATOR, 0, 0);
				// The class list has a specific order so we have to sort at run-time for the UI.
				struct {
					LPCSTR Cls; WORD Idx;
					static int __cdecl Comparer(const void *a, const void *b) { return strcmp(*(char**) a, *(char**) b); }
				} classes[ARRAYSIZE(g_KnownControls)];
				for (UINT i = 0; i < ARRAYSIZE(classes); ++i)
				{
					classes[i].Idx = WORD(i);
					classes[i].Cls = GetFriendlyClassName(g_KnownAtoms[i], g_KnownControls[i].Cls);
				}
				qsort(classes, ARRAYSIZE(classes), sizeof(*classes), classes[0].Comparer);
				for (UINT i = 0; i < ARRAYSIZE(classes); ++i)
				{
					mf = MenuCheckedFlag(g_ForceClass == classes[i].Idx);
					AppendMenuA(hSubMenu, MF_STRING|mf, IDC_FORCECLASS_FIRST + classes[i].Idx, classes[i].Cls);
				}
				AppendMenuA(hMenu, MF_SEPARATOR, 0, 0);
				AppendMenuA(hMenu, MF_STRING|MenuCheckedFlag(g_hLockKbdHook), IDC_LOCKONKEY, "&Lock on CapsLock");
				AppendMenuA(hMenu, MF_SEPARATOR, 0, 0);
				AppendMenuA(hMenu, MF_POPUP, (SIZE_T) (hSubMenu = CreatePopupMenu()), "Always on Top");
				AppendMenuA(hSubMenu, MF_STRING|MenuCheckedFlag(g_AOT == IDC_AOT_NEVER), IDC_AOT_NEVER, "&Never");
				AppendMenuA(hSubMenu, MF_STRING|MenuCheckedFlag(g_AOT == IDC_AOT_INACTIVE), IDC_AOT_INACTIVE, "When &Inactive");
				AppendMenuA(hSubMenu, MF_STRING|MenuCheckedFlag(g_AOT == IDC_AOT_ALWAYS), IDC_AOT_ALWAYS, "&Always");
				AppendMenuA(hMenu, MF_SEPARATOR, 0, 0);
				AppendMenuA(hMenu, MF_STRING, IDC_ABOUT, "About");
				SendMessage(g_hTBar, TB_PRESSBUTTON , IDC_MENU, true);
				TrackPopupMenu(hMenu, tpm_align | TPM_VERTICAL, r.left, r.top, 0, hWnd, NULL);
				SendMessage(g_hTBar, TB_PRESSBUTTON , IDC_MENU, false);
				DestroyMenu(hMenu);
			}
			break;

		case IDC_LOCK:
			LPar = ((HWND)LPar == HWND_MESSAGE) || !SNDMSG(g_hTBar, TB_ISBUTTONCHECKED, IDC_LOCK, 0);
			SNDMSG(g_hTBar, TB_CHECKBUTTON, IDC_LOCK, LPar);
			if (LPar)
				WPar = 0, g_PickerMode |= 0x8000, KillTimer(hWnd, TID_CAPTUREWINDOW);
			else
				WPar = g_PickerMode &= ~0x8000;
			goto doSetPickerMode;

		case IDC_PARENT:
			SendMessage(hWnd, WM_COMMAND, IDC_LOCK, (SIZE_T) HWND_MESSAGE); // Lock
			PickWindowIfValid(GetAncestorParent(g_hSelected));
			break;
		case IDC_CHILD:
			CXX_ATT_FALLTHROUGH;
		case IDC_NEXT:
			CXX_ATT_FALLTHROUGH;
		case IDC_PREV:
			SendMessage(hWnd, WM_COMMAND, IDC_LOCK, (SIZE_T) HWND_MESSAGE); // Lock
			PickWindowIfValid(GetWindow(g_hSelected, LOWORD(WPar) == IDC_CHILD ? GW_CHILD : GW_HWNDNEXT + LOWORD(WPar) - IDC_NEXT));
			break;

		case IDC_AOT_NEVER:
			SetWindowPos(hWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOSIZE), g_AOT = IDC_AOT_NEVER;
			break;
		case IDC_AOT_INACTIVE:
			if (GetForegroundWindow() != hWnd) break;
			CXX_ATT_FALLTHROUGH;
		case IDC_AOT_ALWAYS:
			SetWindowPos(hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOSIZE), g_AOT = LOBYTE(WPar);
			break;

		case IDC_FORCECLASS_NONE:
			g_ForceClass = -1;
			CXX_ATT_FALLTHROUGH;
		case IDC_REFRESH:
			goto doRefresh;

		case IDC_EDITATTRIBUTE:
			LPar = ListView_GetNextItem(g_hList, -1, LVNI_SELECTED);
			if ((int) LPar != -1) ListView_EditLabel(g_hList, (int) LPar);
			break;

		case IDC_COPY:
			goto doCopy;

		case IDC_LOCKONKEY:
			if (g_hLockKbdHook)
				UnhookWindowsHookEx(g_hLockKbdHook), g_hLockKbdHook = 0;
			else
				g_hLockKbdHook = SetWindowsHookEx(WH_KEYBOARD_LL, LockKbdHookProc, NULL, 0);
			break;

		case IDC_ABOUT:
			{
				TCHAR buf[200], bitness = sizeof(void*) * 8;
				TASKDIALOGCONFIG tdc;
				tdc.cbSize = sizeof(tdc);
				tdc.pszVerificationText = tdc.pszExpandedInformation = tdc.pszFooter = 0;
				tdc.pfCallback = 0;
				wsprintf(buf, TEXT("v%u.%u.%u (%d-bit)%hs"), VER_A, VER_B, VER_C, bitness, "\nCopyright (C) Anders Kjersem");
				tdc.hwndParent = hWnd, tdc.cxWidth = 0;
				tdc.pszWindowTitle = L"About";
				tdc.pszMainInstruction = g_AppName, tdc.pszContent = buf;
				tdc.hMainIcon = (HICON) GetClassLongPtr(tdc.hwndParent, GCLP_HICON);
				tdc.dwFlags = TDF_USE_HICON_MAIN|TDF_ALLOW_DIALOG_CANCELLATION|TDF_POSITION_RELATIVE_TO_WINDOW|TDF_SIZE_TO_CONTENT;
				tdc.dwCommonButtons = TDCBF_CLOSE_BUTTON, tdc.cButtons = tdc.cRadioButtons = 0;
				int(WINAPI*TDI)(TASKDIALOGCONFIG*,int*,int*,BOOL*) = GetProcAddr<int(WINAPI*)(TASKDIALOGCONFIG*,int*,int*,BOOL*)>("COMCTL32", (char*) 345);
				if (TDI)
					TDI(&tdc, 0, 0, 0);
				else
					MessageBox(tdc.hwndParent, tdc.pszContent, tdc.pszWindowTitle, MB_OK|MB_ICONINFORMATION);
			}
			break;
		}
		break;

	case WM_APPCOMMAND:
		if (GET_APPCOMMAND_LPARAM(LPar) == APPCOMMAND_BROWSER_REFRESH)
		{
			doRefresh:
			UpdateSummary(g_hSelected);
			return true;
		}
		break;

	case WM_SETFOCUS:
		if (g_hFocus != hWnd) SetFocus(g_hFocus);
		break;

	case WM_ACTIVATE:
		if (!WPar) g_hFocus = GetFocus();
		if (g_AOT == IDC_AOT_INACTIVE) SetWindowPos(hWnd, WPar ? HWND_NOTOPMOST : HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOSIZE);
		break;

	case WM_CONTEXTMENU:
		if ((HWND) WPar == g_hList) DisplayAttributeMenu(LPar);
		break;

	case WM_COPY:
		{
			doCopy:
			TCHAR buf[MAX_PATH];
			LVITEM lvi;
			lvi.mask = LVIF_TEXT|LVIF_PARAM, lvi.iSubItem = 1;
			lvi.pszText = buf, lvi.cchTextMax = ARRAYSIZE(buf);
			lvi.iItem = ListView_GetNextItem(g_hList, -1, LVNI_SELECTED); 
			if (lvi.iItem != -1 && ListView_GetItem(g_hList, &lvi)) ClipboardCopy(lvi.pszText);
		}
		break;

	case WM_QUERYOPEN:
		if (!IsLocked()) SendMessage(hWnd, WM_COMMAND, g_PickerMode, 0); // Restore timer interval.
		break;

	case WM_DPICHANGED:
		g_Dpi = LOWORD(WPar);
		SetWindowPos(hWnd, NULL, ((RECT*)LPar)->left, ((RECT*)LPar)->top, RW(*((RECT*)LPar)), RH(*((RECT*)LPar)), SWP_NOZORDER|SWP_NOACTIVATE);
		RefreshControlMetrics();
		break;
	
	case WM_SETTINGCHANGE:
		RefreshControlMetrics();
		break;

	case WM_CREATE:
		{
			HINSTANCE hInst = ((CREATESTRUCT*)LPar)->hInstance;
			RefreshCachedDpiValues();

			// Pre-compute class info for faster window identification
			for (UINT i = 0; i < ARRAYSIZE(g_KnownControls); ++i)
			{
				TCHAR buf[100], *cls;
				StrCpyRaw(cls = buf, const_cast<LPCSTR>(g_KnownControls[i].Cls));
				hCtl = cls[0] != '#' ? CreateWindow(cls, NULL, WS_CHILD|WS_DISABLED, 0, 0, 0, 0, hWnd, NULL, hInst, NULL) : NULL;
				SIZE_T atom = hCtl ? GetClassLongPtr(hCtl, GCW_ATOM) : 0;
				if (FASTCLASSHASH(cls) == FASTCLASSHASH(TEXT("RichEdit"))) g_RichEditv1Atom = LOWORD(atom);
				g_KnownAtoms[i] = cls[0] != '#' ? LOWORD(atom) : LOWORD(StrToIntPtr(cls + 1));
				SNDMSG(hCtl, WM_CLOSE, 0, 0);
			}

			UINT ctlstyle = WS_CHILD|WS_TABSTOP|WS_VISIBLE|LVS_REPORT|LVS_NOSORTHEADER|LVS_SINGLESEL|LVS_EDITLABELS;
			hCtl = CreateWindowEx(WS_EX_CLIENTEDGE, WC_LISTVIEW, NULL, ctlstyle, 0, 0, 0, 0, hWnd, (HMENU) IDC_LIST, hInst, NULL);
			if ((g_hFocus = g_hList = hCtl) == 0) return Die(hWnd);
			SNDMSG(hCtl, CCM_SETVERSION, 5, 0), SNDMSG(hCtl, CCM_SETVERSION, 6, 0);
			ListView_SetExtendedListViewStyle(hCtl, LVS_EX_FULLROWSELECT|LVS_EX_GRIDLINES|LVS_EX_HEADERDRAGDROP|LVS_EX_INFOTIP|LVS_EX_LABELTIP|LVS_EX_DOUBLEBUFFER);
			LVCOLUMN lvc;
			lvc.mask = LVCF_TEXT|LVCF_SUBITEM|LVCF_WIDTH, lvc.iSubItem = 0, lvc.cx = 500;
			lvc.pszText = TEXT("Attribute"), lvc.iSubItem = 1 + (UINT) SNDMSG(hCtl, LVM_INSERTCOLUMN, lvc.iSubItem, (SIZE_T) &lvc);
			lvc.pszText = TEXT("Value"), lvc.iSubItem = 1 + (UINT) SNDMSG(hCtl, LVM_INSERTCOLUMN, lvc.iSubItem, (SIZE_T) &lvc);
			for (UINT i = 0; i < ARRAYSIZE(g_RowNames); ++i)
			{
				LVITEM lvi;
				lvi.mask = LVIF_TEXT;
				lvi.iItem = i, lvi.iSubItem = 0;
				lvi.pszText = (LPTSTR) const_cast<LPSTR>(g_RowNames[i]);
				SNDMSG(hCtl, LVM_INSERTITEMA, 0, (SIZE_T) &lvi);
			}
			SetWindowPos(ListView_GetToolTips(hCtl), HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOSIZE); // Fix Windows bug where the labeltip is hidden when we are AoT.

			ctlstyle = (g_TBarVert ? CCS_VERT|CCS_LEFT : CCS_TOP)|CCS_NODIVIDER|CCS_NORESIZE|TBSTYLE_TRANSPARENT|TBSTYLE_FLAT|TBSTYLE_LIST|TBSTYLE_TOOLTIPS;
			g_hTBar = hCtl = CreateWindowEx(0, TOOLBARCLASSNAME, NULL, WS_CHILD|WS_TABSTOP|WS_VISIBLE|ctlstyle, 0, 0, 0, 0, hWnd, (HMENU) IDC_TBAR, hInst, NULL);
			if (!hCtl) return Die(hWnd);
			SendMessage(hCtl, TB_BUTTONSTRUCTSIZE, sizeof(TBBUTTON), 0);
			SendMessage(hCtl, TB_SETINDENT, 0, 0);
			SendMessage(hCtl, TB_SETEXTENDEDSTYLE, 0, TBSTYLE_EX_MIXEDBUTTONS); // TBSTYLE_EX_VERTICAL?
			SendMessage(hCtl, TB_SETWINDOWTHEME, 0, (SIZE_T) L"Explorer");
			static const struct { WORD Id; LPCTSTR String; } buttons[] = { 
				{ IDC_MENU, TEXT("Menu") }, { 0 },
				{ IDC_BYMOUSE }, { IDC_BYTL }, { IDC_BYTR }, { IDC_BYBR }, { IDC_BYBL }, { 0 },
				{ IDC_LOCK, TEXT("Lock") }, { 0 },
				{ IDC_PARENT, TEXT("Parent") }, { IDC_CHILD, TEXT("Child") },
				{ IDC_NEXT, TEXT("Next") }, { IDC_PREV, TEXT("Previous") },
			};
			for (UINT vert = g_TBarVert, img = 0, i = 0; i < ARRAYSIZE(buttons); ++i)
			{
				BYTE sep = buttons[i].Id == 0, bs = sep ? BTNS_SEP : BTNS_BUTTON|BTNS_AUTOSIZE|BTNS_NOPREFIX, state = sep ? 0 : TBSTATE_ENABLED;
				TBBUTTON tbb = { sep ? 0 : img, sep ? 0 : buttons[i].Id, state|(vert ? TBSTATE_WRAP : 0), bs };
				tbb.dwData = (SIZE_T) buttons[i].String, img += !sep;
				SNDMSG(hCtl, TB_ADDBUTTONS, 1, (SIZE_T) &tbb);
			}

			g_PickerMode = IDC_BYMOUSE;
		}
		break;

	case WM_DESTROY:
		{
			if (g_hLockKbdHook) UnhookWindowsHookEx(g_hLockKbdHook), g_hLockKbdHook = 0;
			TCHAR ini[MAX_PATH];
			if (GetConfigPath(ini, ARRAYSIZE(ini)))
			{
				SetConfigInt(ini, CONFIG_SECTION, TEXT("AOT"), g_AOT - IDC_AOT_NEVER);
				SetConfigInt(ini, CONFIG_SECTION, TEXT("Pick"), GetPickerType() - IDC_BYMOUSE);
				WINDOWPLACEMENT wp;
				GetWindowPlacement(hWnd, (wp.length = sizeof(wp), &wp));
				WritePrivateProfileStruct(CONFIG_SECTION, TEXT("WP"), &wp, sizeof(wp), ini);
			}
			PostMessage(hWnd, WM_QUIT, WPar, LPar);
		}
		break;
	}
	return DefWindowProc(hWnd, Msg, WPar, LPar);
}


template<class T> static inline int App()
{
	HINSTANCE hInst = HINST_APPLICATION;
	g_WindowInfo.cbSize = sizeof(WINDOWINFO);
	g_hDesktop = GetDesktopWindow();
	g_AdjustWindowRectExForDpi = GetProcAddr("USER32", "AdjustWindowRectExForDpi", FallbackAdjustWindowRectExForDpi);
	g_GetSystemMetricsForDpi = GetProcAddr("USER32", "GetSystemMetricsForDpi", FallbackGetSystemMetricsForDpi);
	g_GetDpiForWindow = GetProcAddr("USER32", "GetDpiForWindow", FallbackGetDpiForWindow);
	g_SetWindowTheme = sizeof(void*) > 4 ? SetWindowTheme : GetProcAddr("UXTHEME", "SetWindowTheme", FallbackSetWindowTheme);
	g_GetDpiForMonitor = GetProcAddr("SHCORE", "GetDpiForMonitor", FallbackGetDpiForMonitor);
	g_SysHasPerMonitorDpi = g_GetDpiForMonitor != FallbackGetDpiForMonitor;
	g_DwmGetWindowAttribute = GetProcAddr("DWMAPI", "DwmGetWindowAttribute", FallbackDwmGetWindowAttribute);
	g_SysHasDWM = g_DwmGetWindowAttribute != FallbackDwmGetWindowAttribute;

	INITCOMMONCONTROLSEX icce = { 8, ICC_LINK_CLASS|ICC_NATIVEFNTCTL_CLASS|ICC_PAGESCROLLER_CLASS|ICC_INTERNET_CLASSES|ICC_COOL_CLASSES|ICC_USEREX_CLASSES|ICC_DATE_CLASSES };
	InitCommonControlsEx(&icce);
	LoadLibraryA("RichEd32");
	VerifyStyles();

	HICON hIco = LoadImage(hInst, MAKEINTRESOURCE(IDI_APPICON), IMAGE_ICON, 0, 0, LR_DEFAULTSIZE|LR_SHARED);
	HCURSOR hCur = LoadImage(NULL, MAKEINTRESOURCE(IDC_ARROW), IMAGE_CURSOR, 0, 0, LR_DEFAULTSIZE|LR_SHARED);
	WNDCLASS wc = { 0, MainWndProc, 0, 0, hInst, hIco, hCur, (HBRUSH)(COLOR_3DFACE + 1), NULL, g_AppName };
	if (!RegisterClass(&wc)) return ErrorDlg();

	g_hMain = CreateWindowEx(WS_EX_APPWINDOW, g_AppName, g_AppName, WS_CLIPCHILDREN|WS_TILEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, NULL, NULL, hInst, NULL);
	if (!g_hMain) return ErrorDlg();
	InitializeMainWindow();

	MSG msg;
	while ((int) GetMessage(&msg, NULL, 0, 0) > 0)
	{
		if ((msg.message == WM_KEYDOWN) | (msg.message == WM_SYSKEYDOWN))
		{
			UINT alt = msg.message == WM_SYSKEYDOWN, ctr = GetKeyState(VK_CONTROL) < 0, shf = GetKeyState(VK_SHIFT) < 0, cmd;
			UINT accel = MAKEWORD(BYTE(msg.wParam), (alt ? MOD_ALT : 0) | (ctr ? MOD_CONTROL : 0) | (shf ? MOD_SHIFT : 0)), i;
			for (i = 0, cmd = 0; i < ARRAYSIZE(g_AccelTable); ++i)
				if (g_AccelTable[i].Accel == accel)
					cmd = g_AccelTable[i].Cmd;
			if (cmd)
			{
				bool valid = -1L == (UINT) SNDMSG(g_hTBar, TB_COMMANDTOINDEX, cmd, 0);
				if (valid || SNDMSG(g_hTBar, TB_ISBUTTONENABLED, cmd, 0))
					SNDMSG(g_hMain, WM_COMMAND, cmd, 0);
				else
					MessageBeep(MB_ICONWARNING);
				continue;
			}
		}
		if (IsDialogMessage(g_hMain, &msg)) continue;
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	return (UINT) msg.wParam;
}

EXTERN_C DECLSPEC_NORETURN void WinMainCRTStartup()
{
	ExitProcess(App<TCHAR>());
}
