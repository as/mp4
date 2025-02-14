// quick usage guide
/*
 # Print ept bmdt and sequence number without other output
 mp4 -q 0.m4s

 # Read 0.m4s, modify seq, bmdt, ept, output edited file to standard output
 mp4 -seq 2 -bmdt 30720 -ept 31744 0.m4s

 # Read files with the pattern 2_%03d.m4s 4_%03.m4s sequentially, using
 # the first file, 2_001.m4s as a baseline sequence and timestamp, then, for
 # subsequent files 2_002.m4s 2_003.m4s ... 4_001.m4s etc, generate the
 # commands to update those timestamps and sequence numbers and emit
 # them to a new location as if they were part of a sequential series of fragments
 # in the same period
  mp4 -fmt '../dash3/2_%03d.m4s' -renum -q 2_0*.m4s 4_*m4s
*/
package main

import (
	//"bufio"
	"bytes"
	"encoding/binary"
	"fmt"
	"io"
	"io/ioutil"
	"os"

	"flag"
)

var (
	quiet   = flag.Bool("q", false, "print only the three stooges (presentation/decode/sequence) to stderr")
	fept    = flag.Int("ept", -1, "modify sidx.earliest_presentation_time if not negative")
	fseq    = flag.Int("seq", -1, "modify moof.mfhd.sequence_number if not negative")
	fbmdt   = flag.Int("bmdt", -1, "modify moof.traf.base_media_decode_time if not negative")
	fm      = flag.String("fmt", "", "generate a i/o redirect into the formatted file spec like 1_%03d.m4s")
	initbox = flag.String("init", "", "write the init box to this location instead of stdout")
	fast    = flag.Bool("fast", false, "when using init, prevents parsing the mp4 past the moov atom")

	renum   = flag.Bool("renum", false, "assume input files are related fragments and realign based on first one and subsequent durations computed")
	debug   = flag.Bool("debug", false, "debug output")
	verbose = flag.Bool("v", false, "verbose output")
	clamp   = flag.Int("clamp", 9999999, "limit the number of track identifiers printed in verbose mode, setting it to a number like 5 will only print the first and last 5 track info fields")
)

var level = -1
var tab = "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"

var (
	stdout = io.Writer(os.Stdout)
	stderr = io.Writer(os.Stderr)
)

var (
	pept, pseq, pbmdt = 0, 0, 0
	samples           = 0
	video             = false
)

var (
	// last default segment size (dss) and duration (dsd)
	lastdss, lastdsd uint32
	lasttrack        uint32

	nread    = 0
	moovsize = 0

	// track to timescale, as parsed from the moov atom
	track2ts   = map[uint32]uint32{}
	track2dur  = map[uint32]int{}
	track2size = map[uint32]int{}
	track2ept  = map[uint32]uint64{}
	sidxctr    uint64
	tracksize  int
)

func makesidx(t uint32) (atom []byte) {
	defer func() { sidxctr += uint64(len(atom)) }()
	a := NewAtom(
		"sidx", uint32(1<<24), // 4+4+4 (12)
		t, track2ts[t], // 4+4 (20)
		track2ept[t], sidxctr, // 8+8 (36)
		uint32(1),                                                   // 2 + 2 (40)
		uint32(tracksize), uint32(track2dur[t]), uint32(0x80000000), // 4+4+4 (52)
	)
	return a.Bytes()
}

type hdr struct {
	Len  int32
	Type [4]byte
}

func (h *hdr) String() string { return string(h.Type[:]) }

type Atom struct {
	hdr
	data bytes.Buffer
}

func (a *Atom) Bytes() []byte {
	p := a.data.Bytes()
	binary.BigEndian.PutUint32(p[:4], uint32(len(p)))
	return p
}

func (a *Atom) Add(data ...any) {
	encode(&a.data, data...)
}

func NewAtom(kind string, data ...any) (a Atom) {
	a.data.Write(make([]byte, 8))
	copy(a.data.Bytes()[4:8], kind)
	a.Add(data...)
	return a
}

var multiout = false

func main() {
	flag.Parse()
	if *quiet {
		stdout = ioutil.Discard
		stderr = ioutil.Discard
	} else if *initbox != "" {
		fd, err := os.Create(*initbox)
		if err != nil {
			panic(err)
		}
		defer fd.Close()
		stdout = fd
		multiout = true
	}
	pept, pseq, pbmdt = 0, 0, 0
	if len(flag.Args()) == 0 {
		read("", Counter{os.Stdin})
		fmt.Fprintf(os.Stderr, "/dev/stdin seq=%v bmdt=%v ept=%v\n", pseq, pbmdt, pept)
	} else {
		cur := struct{ seq, bmdt, ept int }{}
		for i, f := range flag.Args() {
			pept, pseq, pbmdt, samples = 0, 0, 0, 0
			video = false
			fd, _ := os.Open(f)
			read("", Counter{fd})
			fd.Close()
			if (i == 0 && *renum) || !*renum {
				cur.seq = pseq
				cur.bmdt = pbmdt
				cur.ept = pept
			}
			mul := 512
			if !video {
				mul = 1024
			}
			dur := samples * mul
			redir := ""
			if *fm != "" {
				redir = "> " + fmt.Sprintf(*fm, i+1)
			}
			fmt.Fprintf(os.Stderr, "%s -seq %v -bmdt %v -ept %v %s %s# %d samples %d dur video=%v\n", os.Args[0], cur.seq, cur.bmdt, cur.ept, f, redir, samples, samples*mul, video)
			if *renum {
				cur.seq++
				cur.bmdt += dur
				cur.ept += dur
			}
		}
	}
	for k, v := range track2ts {
		fmt.Fprintf(os.Stderr, "track=%d ts=%d dur=%d size=%d\n", k, v, track2dur[k], uint32(tracksize))
	}
	sidx := make([][]byte, len(track2ts))
	for i := len(track2ts) - 1; i >= 0; i-- {
		sidx[i] = makesidx(uint32(i + 1))
		fmt.Fprintf(os.Stderr, "track=%d sidx size=%d val=%x\n", i+1, len(sidx[i]), sidx[i])
	}
}

func encode(w io.Writer, v ...any) error {
	for _, v := range v {
		switch v := v.(type) {
		case io.Reader:
			io.Copy(w, v)
		case []byte:
			io.Copy(w, bytes.NewReader(v))
		case *Varint:
			binary.Write(w, binary.BigEndian, v.Value())
		case Varint:
			binary.Write(w, binary.BigEndian, v.Value())
		case any:
			binary.Write(w, binary.BigEndian, v)
		}
	}
	return nil
}

func bread(r io.Reader, h *hdr, dst ...any) error {
	if dst == nil {
		dst = []any{h}
	}
	for _, dst := range dst {
		if dst == nil {
			dst = h
		}
		if v, ok := dst.(*Varint); ok {
			dst = v.Ptr()
		}
		err := binary.Read(r, binary.BigEndian, dst)
		if err != nil {
			return err
		}
	}
	return nil
}

type parser struct {
	level int

	in  io.Reader
	err io.Reader
	out io.Writer
}

func (p *parser) skip(n int32) {
	io.CopyN(p.out, p.in, int64(n))
}

func read(dir string, r io.Reader) {
	level++
	defer func() { level-- }()
	h := hdr{}
	//r = Counter{bufio.NewReader(r)}
	skip := func(n int32) {
		io.CopyN(stdout, r, int64(n))
	}
	write := func(v ...any) error {
		stdout := &Writer{Writer: stdout}
		return encode(stdout, v...)
	}
	bread := func(dst ...any) error {
		return bread(r, &h, dst...)
	}
	cbread := func(list []any, cond bool, dst ...any) []any {
		if cond {
			bread(dst...)
			list = append(list, dst...)
		}
		return list
	}
	var err error
	for {
		if err = bread(nil); err != nil {
			break
		}
		t := string(h.Type[:])
		printf := func(fm string, v ...any) {
			v = append([]any{dir, tab[:level]}, v...)
			fmt.Fprintf(stderr, "%s%s "+fm, v...)
		}
		vprintf := func(fm string, v ...any) {
			if *verbose {
				printf(fm, v...)
			}
		}
		printf("%q	%d\n", t, h.Len)
		switch t {
		case "tkhd":
			vflag, track := uint32(0), uint32(0)
			bread(&vflag)
			ctime := Varint{is32: vflag>>24 == 0}
			mtime := ctime
			res := uint32(0)
			dur := ctime

			res2 := uint64(0)
			layer := uint16(0)
			altgroup := uint16(0)
			vol := uint16(0)
			res3 := uint16(0)
			matrix := [9 * 4]byte{}
			dx, dy := int32(0), int32(0)
			list := cbread([]any{h, vflag}, true, &ctime, &mtime, &track, &res, &dur,
				&res2, &layer, &altgroup, &vol, &res3, matrix[:], &dx, &dy)

			lasttrack = track
			printf("matrix=%+v \n", matrix[:])
			printf("%q	vflag=%08x track=%v ctime=%s mtime=%s dur=%s vol=%v size=%dx%d\n", t, vflag, track,
				ctime, mtime, dur, vol, dx>>16, dy>>16)
			write(list...)
		case "tfhd":
			vflag, track, bdo, sdi, dsd, dss, dsf := uint32(0), uint32(0), uint64(0), uint32(0), uint32(0), uint32(0), uint32(0)
			bread(&vflag, &track)
			list := []any{h, vflag, track}
			list = cbread(list, vflag&0x000001 != 0, &bdo)
			list = cbread(list, vflag&0x000002 != 0, &sdi)
			list = cbread(list, vflag&0x000008 != 0, &dsd)
			list = cbread(list, vflag&0x000010 != 0, &dss)
			list = cbread(list, vflag&0x000020 != 0, &dsf)
			lastdss = dss
			lastdsd = dsd
			printf("%q	vflag=%08x track=%v bdo=%v sdi=%v dsd=%v dss=%v dsf=%v\n", t, vflag, track, bdo, sdi, dsd, dss, dsf)
			lasttrack = track
			write(list...)
		case "traf":
			write(h)
			read(dir+"/"+t, io.LimitReader(r, int64(h.Len)-8))
		case "mdat":
			write(h)
			tracksize += int(h.Len)
			skip(h.Len - 8)
		case "moof":
			write(h)
			tracksize += int(h.Len)
			read(dir+"/"+t, io.LimitReader(r, int64(h.Len)-8))
		case "trun":
			vflag, nsamples, dof, fsf := uint32(0), uint32(0), uint32(0), uint32(0)
			bread(&vflag, &nsamples)
			list := []any{h, vflag, nsamples}
			list = cbread(list, vflag&0x000001 != 0, &dof)
			list = cbread(list, vflag&0x000004 != 0, &fsf)

			printf("%q	before vflag=%08x samples=%d dof=%d fsf=%d\n", t, vflag, nsamples, dof, fsf)
			if dof > 0 {
				//dof -= uint32(moovsize)
			}

			totalsize := 0
			totaldur := 0
			for i := 0; i < int(nsamples); i++ {
				dur, size, tflag, scto := uint32(0), uint32(0), uint32(0), uint32(0)
				list = cbread(list, vflag&0x000100 != 0, &dur)
				list = cbread(list, vflag&0x000200 != 0, &size)
				list = cbread(list, vflag&0x000400 != 0, &tflag)
				list = cbread(list, vflag&0x000800 != 0, &scto)
				if dur == 0 {
					dur = lastdsd
				}
				if size == 0 {
					size = lastdss
				}
				totaldur += int(dur)
				totalsize += int(size)
				printf("\t%q	sample=%d dur=%v size=%v tflag=%x scto=%v\n", t, i, dur, size, tflag, scto)
			}
			track2size[lasttrack] = totalsize
			track2dur[lasttrack] = totaldur
			printf("%q	track=%d vflag=%08x samples=%d totalsize=%d totaldur=%d\n", t, lasttrack, vflag, nsamples, totalsize, totaldur)
			write(list...)
		case "tfdt":
			ver, flags := uint8(0), [3]byte{}
			bread(&ver, &flags)
			bmdt := Varint{is32: ver == 0}
			bread(&bmdt)
			pbmdt = bmdt.Int()
			ts := track2ts[lasttrack]
			printf("%q	track=%d ver=%d flags=%x ts=%v bmdt=%d (%fs)\n", t, lasttrack, ver, flags, ts, bmdt.Int(), float64(bmdt.Int())/float64(ts))
			if *fbmdt > 0 {
				bmdt.Set(*fbmdt)
			}
			track2ept[lasttrack] = uint64(bmdt.Int())
			write(h, ver, flags, bmdt)
		case "sidx":
			vflag, rid, ts := uint32(0), uint32(0), uint32(0)
			list := []any{h}
			list = cbread(list, true, &vflag, &rid, &ts)

			ept := Varint{is32: vflag>>24 == 0}
			fo := Varint{is32: vflag>>24 == 0}
			res, nent := int16(0), int16(0)

			list = cbread(list, true, &ept, &fo, &res, &nent)
			pept = ept.Int()

			printf("%q	ver=%v flags=%x rid=%d ts=%d ept=%d fo=%d nent=%v\n",
				t, vflag>>24, vflag&0xFFFFFF, rid, ts, ept.Int(), fo.Int(), nent)
			for i := 1; i <= int(nent); i++ {
				typesize, dur, sap := uint32(0), uint32(0), uint32(0)
				list = cbread(list, true, &typesize, &dur, &sap)
				typ, size := typesize>>31, typesize<<1>>1
				printf("	%q	ent=%d type=%v size=%d dur=%d (%fs) sap=%x\n", t, i, typ, size, dur, float64(dur)/float64(ts), sap)
			}

			if *fept > 0 {
				ept.Set(*fept)
			}
			write(list...)
		case "mfhd":
			seq := uint64(0)
			bread(&seq)
			printf("%q	seq=%d\n", t, seq)
			pseq = int(seq)
			if *fseq > 0 {
				seq = uint64(*fseq)
			}
			write(h, seq)
		case "moov":
			write(h)
			read(dir+"/"+t, io.LimitReader(r, int64(h.Len)-8))
			println("read", nread, "bytes")
			moovsize = nread
			if multiout {
				stdout = os.Stdout
				if *fast {
					io.Copy(stdout, r)
					os.Exit(0)
				}
			}
		case "stsc": // sample to chunk offset (absolute file offset)
			// note: a chunk contains one or more samples
			vflag, nelem := uint32(0), uint32(0)
			bread(&vflag, &nelem)
			write(h, vflag, nelem)
			first, samples, sdi := uint32(0), uint32(0), uint32(0)
			printf("%q	vflags=%x nelem=%d \n", t, vflag, nelem)
			for i := 1; i <= int(nelem); i++ {
				bread(&first, &samples, &sdi)
				if i < 5 || i > int(nelem)-5 {
					vprintf("%q	i=%d first=%d samples=%d sdi=%d \n", t, i, first, samples, sdi)
				}
				write(first, samples, sdi)
			}
		case "stco":
			vflag, nelem := uint32(0), uint32(0)
			bread(&vflag, &nelem)
			write(h, vflag, nelem)
			printf("%q	vflags=%x nelem=%d \n", t, vflag, nelem)
			co := uint32(0)
			for i := 1; i <= int(nelem); i++ {
				bread(&co)
				if i < *clamp || i > int(nelem)-*clamp {
					vprintf("%q	sample=%d coffset=%d\n", t, i, co)
				}
				write(co)
			}
		case "stsz": // sample size box
			vflag, size, count := uint32(0), uint32(0), uint32(0)
			bread(&vflag, &size, &count)
			write(h, vflag, size, count)
			printf("%q	vflags=%x size=%d count=%d\n", t, vflag, size, count)
			if size == uint32(0) {
				for i := 1; i <= int(count); i++ {
					bread(&size)
					if i < *clamp || i > int(count)-*clamp {
						vprintf("%q	sample=%d size=%d\n", t, i, size)
					}
					write(size)
				}
			}
		case "stts": // time to sample box
			vflag, count := uint32(0), uint32(0)
			bread(&vflag, &count)
			write(h, vflag, count)
			scount, sdelta := uint32(0), uint32(0)
			printf("%q	vflags=%x count=%d \n", t, vflag, count)
			for i := 1; i <= int(count); i++ {
				bread(&scount, &sdelta)
				if i < *clamp || i > int(count)-*clamp {
					vprintf("%q	entry=%d scount=%d sdelta=%d\n", t, i, scount, sdelta)
				}
				write(scount, sdelta)
			}
		case "stss": // sync sample box
			vflag, count, isample := uint32(0), uint32(0), uint32(0)
			bread(&vflag, &count)
			write(h, vflag, count)
			printf("%q	vflags=%x isamples=%d\n", t, vflag, count)
			for i := 0; i < int(count); i++ {
				bread(&isample)
				if i < *clamp || i > int(count)-*clamp {
					vprintf("%q	entry=%d iframe=%d\n", t, i, isample)
				}
				write(isample)
			}

		case "trak", "mdia", "minf", "stbl":
			write(h)
			read(dir+"/"+t, io.LimitReader(r, int64(h.Len)-8))
		case "mdhd":
			vflag, ts, lang := uint32(0), uint32(0), uint32(0)
			bread(&vflag)
			ctime := Varint{is32: vflag>>24 == 0}
			mtime := ctime
			dur := ctime

			list := []any{h, vflag}
			list = cbread(list, true, &ctime, &mtime, &ts, &dur, &lang)

			track2ts[lasttrack] = ts
			printf("%q	vflags=%x ctime=%d mtime=%d ts=%d dur=%d lang=%x\n",
				t, vflag, ctime.Int(), mtime.Int(), ts, dur.Int(), lang)
			write(list...)
		default:
			write(h)
			skip(h.Len - 8)
		}
	}
}

type Counter struct {
	io.Reader
}

func (r Counter) Read(p []byte) (n int, err error) {
	n, err = r.Reader.Read(p)
	nread += n
	return
}

type Writer struct {
	cum int
	n   int
	x   []byte
	io.Writer
}

func (w *Writer) Write(p []byte) (n int, err error) {
	n, err = w.Writer.Write(p)
	w.n += n
	w.cum += n
	w.x = p[:n]
	return
}

func (w *Writer) Cum() int {
	return w.cum
}

func (w *Writer) Lap() int {
	n := w.n
	w.n = 0
	return n
}

type Varint struct {
	is32  bool
	val32 uint32
	val64 uint64
}

func (v Varint) Int() int {
	if v.is32 {
		return int(v.val32)
	}
	return int(v.val64)
}

func (v Varint) String() string {
	return fmt.Sprint(v.Int())
}

func (v *Varint) Set(val int) {
	if v.is32 {
		v.val32 = uint32(val)
	}
	v.val64 = uint64(val)
}

func (v *Varint) Ptr() interface{} {
	if v.is32 {
		return &v.val32
	}
	return &v.val64
}

func (v Varint) Value() interface{} {
	if v.is32 {
		return v.val32
	}
	return v.val64
}
