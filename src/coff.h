#pragma once
#include <tl/file.h>
#include <tl/string.h>
#include <tl/block_list.h>
#define NOMINMAX
#include <Windows.h>
using namespace tl;

// https://docs.microsoft.com/en-us/windows/win32/debug/pe-format

namespace coff {

#pragma pack(push, 1)
struct Header {
	u16 Machine = {};
	u16 NumberOfSections = {};
	u32 TimeDateStamp = {};
	u32 PointerToSymbolTable = {};
	u32 NumberOfSymbols = {};
	u16 SizeOfOptionalHeader = {};
	u16 Characteristics = {};
};
struct Section {
	char Name[8] = {};
	u32 VirtualSize = {};
	u32 VirtualAddress = {};
	u32 SizeOfRawData = {};
	u32 PointerToRawData = {};
	u32 PointerToRelocations = {};
	u32 PointerToLinenumbers = {};
	u16 NumberOfRelocations = {};
	u16 NumberOfLinenumbers = {};
	u32 Characteristics = {};
};
struct Symbol {
	char Name[8]            = {};
	u32  Value              = {};
	s16  SectionNumber      = {};
	u16  Type               = {};
	u8   StorageClass       = {};
	u8   NumberOfAuxSymbols = {};
};
struct Relocation {
	u32 VirtualAddress   = {};
	u32 SymbolTableIndex = {};
	u16 Type             = {};
};
struct LineNumber {
	u32 type = {};
	u16 line = {};
};
#pragma pack(pop)

struct Writer {
	struct Section;
	struct Symbol;
	struct Relocation;

	struct Section {
		Span<utf8> name;
		BlockList<Relocation> relocations;
		Span<u8> data;
		u32 Characteristics = {};
		List<LineNumber> line_numbers;
	};
	struct Symbol {
		Span<utf8> name;
		u32        Value              = {};
		Section   *section            = 0;
		u16        Type               = {};
		u8         StorageClass       = {};
		u8         NumberOfAuxSymbols = {};
	};
	struct Relocation {
		u32     offset = {};
		Symbol *symbol = 0;
		u16     type   = 0;
	};

	u16 Machine = {};
	u32 TimeDateStamp = {};
	u16 SizeOfOptionalHeader = {};
	u16 Characteristics = {};
	BlockList<Section> sections;
	BlockList<Symbol> symbols;

	inline static constexpr Section *undefined_section = (Section *)0;
	inline static constexpr Section *absolute_section  = (Section *)-1;
	inline static constexpr Section *debug_section     = (Section *)-2;
};

struct Reader {
	Buffer buffer;

	Header *header = 0;
	Span<Section> sections;
	Span<Symbol> symbols;
	char *string_table = 0;
};

inline Span<utf8> machine_as_string(u16 machine) {
	switch (machine) {
#define C(x) case IMAGE_FILE_MACHINE_##x: return u8#x##s;
		C(UNKNOWN    )
		C(TARGET_HOST)
		C(I386       )
		C(R3000      )
		C(R4000      )
		C(R10000     )
		C(WCEMIPSV2  )
		C(ALPHA      )
		C(SH3        )
		C(SH3DSP     )
		C(SH3E       )
		C(SH4        )
		C(SH5        )
		C(ARM        )
		C(THUMB      )
		C(ARMNT      )
		C(AM33       )
		C(POWERPC    )
		C(POWERPCFP  )
		C(IA64       )
		C(MIPS16     )
		C(ALPHA64    )
		C(MIPSFPU    )
		C(MIPSFPU16  )
		C(TRICORE    )
		C(CEF        )
		C(EBC        )
		C(AMD64      )
		C(M32R       )
		C(ARM64      )
		C(CEE        )
#undef C
	}
	return u8"?"s;
}
inline List<utf8> header_characteristics_to_string(u16 characteristics) {
	StringBuilder builder;
	builder.allocator = temporary_allocator;

#define C(x) if(characteristics&IMAGE_FILE_##x){if(builder.count())append(builder,'|');append(builder,#x);}
	C(RELOCS_STRIPPED        )
	C(EXECUTABLE_IMAGE       )
	C(LINE_NUMS_STRIPPED     )
	C(LOCAL_SYMS_STRIPPED    )
	C(AGGRESIVE_WS_TRIM      )
	C(LARGE_ADDRESS_AWARE    )
	C(BYTES_REVERSED_LO      )
	C(32BIT_MACHINE          )
	C(DEBUG_STRIPPED         )
	C(REMOVABLE_RUN_FROM_SWAP)
	C(NET_RUN_FROM_SWAP      )
	C(SYSTEM                 )
	C(DLL                    )
	C(UP_SYSTEM_ONLY         )
	C(BYTES_REVERSED_HI      )
#undef C

	return (List<utf8>)to_string(builder);
}
inline List<utf8> section_characteristics_to_string(u32 characteristics) {
	StringBuilder builder;
	builder.allocator = temporary_allocator;

#define C(x) if(characteristics&IMAGE_SCN_##x){if(builder.count())append(builder,'|');append(builder,#x);}
#define A(x) case IMAGE_SCN_##x:if(builder.count())append(builder,'|');append(builder,#x);break;
		C(TYPE_NO_PAD             )
		C(CNT_CODE                )
		C(CNT_INITIALIZED_DATA    )
		C(CNT_UNINITIALIZED_DATA  )
		C(LNK_OTHER               )
		C(LNK_INFO                )
		C(LNK_REMOVE              )
		C(LNK_COMDAT              )
		C(NO_DEFER_SPEC_EXC       )
		C(GPREL                   )
		C(MEM_FARDATA             )
		C(MEM_PURGEABLE           )
		C(MEM_16BIT               )
		C(MEM_LOCKED              )
		C(MEM_PRELOAD             )
		switch (characteristics & IMAGE_SCN_ALIGN_MASK) {
			A(ALIGN_1BYTES            )
			A(ALIGN_2BYTES            )
			A(ALIGN_4BYTES            )
			A(ALIGN_8BYTES            )
			A(ALIGN_16BYTES           )
			A(ALIGN_32BYTES           )
			A(ALIGN_64BYTES           )
			A(ALIGN_128BYTES          )
			A(ALIGN_256BYTES          )
			A(ALIGN_512BYTES          )
			A(ALIGN_1024BYTES         )
			A(ALIGN_2048BYTES         )
			A(ALIGN_4096BYTES         )
			A(ALIGN_8192BYTES         )
		}
		C(LNK_NRELOC_OVFL         )
		C(MEM_DISCARDABLE         )
		C(MEM_NOT_CACHED          )
		C(MEM_NOT_PAGED           )
		C(MEM_SHARED              )
		C(MEM_EXECUTE             )
		C(MEM_READ                )
		C(MEM_WRITE               )
#undef A
#undef C

	return (List<utf8>)to_string(builder);
}
inline Span<utf8> storage_class_as_string(u8 storage_class) {
	switch (storage_class) {
#define C(x) case IMAGE_SYM_CLASS_##x: return u8#x##s;
		C(END_OF_FUNCTION )
		C(NULL            )
		C(AUTOMATIC       )
		C(EXTERNAL        )
		C(STATIC          )
		C(REGISTER        )
		C(EXTERNAL_DEF    )
		C(LABEL           )
		C(UNDEFINED_LABEL )
		C(MEMBER_OF_STRUCT)
		C(ARGUMENT        )
		C(STRUCT_TAG      )
		C(MEMBER_OF_UNION )
		C(UNION_TAG       )
		C(TYPE_DEFINITION )
		C(UNDEFINED_STATIC)
		C(ENUM_TAG        )
		C(MEMBER_OF_ENUM  )
		C(REGISTER_PARAM  )
		C(BIT_FIELD       )
		C(FAR_EXTERNAL    )
		C(BLOCK           )
		C(FUNCTION        )
		C(END_OF_STRUCT   )
		C(FILE            )
		C(SECTION         )
		C(WEAK_EXTERNAL   )
		C(CLR_TOKEN       )
#undef C
	}
	return u8"?"s;
}
inline Span<utf8> relocation_type_amd64_as_string(u8 relocation_type) {
	IMAGE_REL_AMD64_ABSOLUTE;
	switch (relocation_type) {
#define C(x) case IMAGE_REL_AMD64_##x: return u8#x##s;
		C(ABSOLUTE    )
		C(ADDR64      )
		C(ADDR32      )
		C(ADDR32NB    )
		C(REL32       )
		C(REL32_1     )
		C(REL32_2     )
		C(REL32_3     )
		C(REL32_4     )
		C(REL32_5     )
		C(SECTION     )
		C(SECREL      )
		C(SECREL7     )
		C(TOKEN       )
		C(SREL32      )
		C(PAIR        )
		C(SSPAN32     )
		C(EHANDLER    )
		C(IMPORT_BR   )
		C(IMPORT_CALL )
		C(CFG_BR      )
		C(CFG_BR_REX  )
		C(CFG_CALL    )
		C(INDIR_BR    )
		C(INDIR_BR_REX)
		C(INDIR_CALL  )
		C(INDIR_BR_SWITCHTABLE_FIRST)
		C(INDIR_BR_SWITCHTABLE_LAST )

#undef C
	}
	return u8"?"s;
}

Span<char> get_name(char (&name)[8], Reader reader) {
	if (Span(name, 4) == "\0\0\0\0"s)
		return as_span(reader.string_table + *(u32 *)(name + 4));
	else
		if (name[7] == 0)
			return as_span(name);
		else
			return Span(name, 8);
}

}

namespace tl {

inline umm append(StringBuilder &builder, coff::Reader reader) {
	auto header = *reader.header;

	umm result = 0;
	result += append       (builder, "Header:\n");
	result += append_format(builder, "  Machine:                 {}\n", coff::machine_as_string(header.Machine));
	result += append_format(builder, "  Number of sections:      {}\n", header.NumberOfSections);
	result += append_format(builder, "  Time date stamp:         {}\n", header.TimeDateStamp);
	result += append_format(builder, "  Pointer to symbol table: {}\n", header.PointerToSymbolTable);
	result += append_format(builder, "  Number of symbols:       {}\n", header.NumberOfSymbols);
	result += append_format(builder, "  Size of optional header: {}\n", header.SizeOfOptionalHeader);
	result += append_format(builder, "  Characteristics:         {}\n", with(temporary_allocator, coff::header_characteristics_to_string(header.Characteristics)));

	result += append(builder, "Sections:\n");
	for (umm index = 0; index < reader.sections.count; ++index) {
		auto section = reader.sections[index];
		result += append_format(builder, " #{}\n", index + 1);
		result += append_format(builder, "  Name:                   \"{}\"\n", coff::get_name(section.Name, reader));
		result += append_format(builder, "  Virtual size:           {}\n", section.VirtualSize);
		result += append_format(builder, "  Virtual address:        {}\n", section.VirtualAddress);
		result += append_format(builder, "  Size of raw data:       {}\n", section.SizeOfRawData);
		result += append_format(builder, "  Pointer to raw data:    {}\n", section.PointerToRawData);
		result += append_format(builder, "  Pointer to relocations: {}\n", section.PointerToRelocations);
		result += append_format(builder, "  Pointer to linenumbers: {}\n", section.PointerToLinenumbers);
		result += append_format(builder, "  Number of relocations:  {}\n", section.NumberOfRelocations);
		result += append_format(builder, "  Number of linenumbers:  {}\n", section.NumberOfLinenumbers);
		result += append_format(builder, "  Characteristics:        {}\n", with(temporary_allocator, coff::section_characteristics_to_string(section.Characteristics)));
		result += append_format(builder, "  Data:                   {}\n", FormatSpan<u8, FormatInt<u8>>{.value=Span(reader.buffer.data + section.PointerToRawData, section.SizeOfRawData), .format=FormatInt<u8>{.radix=16,.leading_zero_count=2}});
		result += append       (builder, "  Relocations:\n");
		result += append_format(builder, "    {} {} {}\n",
			Format<const char *, char>("VirtualAddress",   {FormatAlign_left, ' ', 20}),
			Format<const char *, char>("SymbolTableIndex", {FormatAlign_left, ' ', 20}),
			Format<const char *, char>("Type",             {FormatAlign_left, ' ', 20}));
		auto section_relocations = Span((coff::Relocation *)(reader.buffer.data + section.PointerToRelocations), section.NumberOfRelocations);
		for (auto relocation : section_relocations) {
			result += append_format(builder, "    {} {} {}\n",
				Format<u32, char>(relocation.VirtualAddress,     {FormatAlign_left, ' ', 20}),
				Format<u32, char>(relocation.SymbolTableIndex+1, {FormatAlign_left, ' ', 20}),
				Format<Span<utf8>, char>(coff::relocation_type_amd64_as_string(relocation.Type), {FormatAlign_left, ' ', 20}));
		}
		result += append       (builder, "  Line numbers:\n");
		result += append_format(builder, "    {} {}\n",
			Format<const char *, char>("Type",   {FormatAlign_left, ' ', 20}),
			Format<const char *, char>("Line",   {FormatAlign_left, ' ', 20}));
		auto section_line_numbers = Span((coff::LineNumber *)(reader.buffer.data + section.PointerToLinenumbers), section.NumberOfLinenumbers);
		for (auto line_number : section_line_numbers) {
			result += append_format(builder, "    {} {}\n",
				Format<u32, char>(line_number.type,     {FormatAlign_left, ' ', 20}),
				Format<u32, char>(line_number.line,     {FormatAlign_left, ' ', 20}));
		}
	}

	result += append(builder, "Symbols:\n");
	for (umm index = 0; index < reader.symbols.count; ++index) {
		auto symbol = reader.symbols[index];
		result += append_format(builder, " #{}\n", index + 1);

		result += append_format(builder, "  Name:                  \"{}\"\n", coff::get_name(symbol.Name, reader));
		result += append_format(builder, "  Value:                 {}\n", symbol.Value);
		switch (symbol.SectionNumber) {
			case IMAGE_SYM_UNDEFINED: result += append       (builder, "  Section number:        UNDEFINED\n"); break;
			case IMAGE_SYM_ABSOLUTE:  result += append       (builder, "  Section number:        ABSOLUTE\n"); break;
			case IMAGE_SYM_DEBUG:     result += append       (builder, "  Section number:        DEBUG\n"); break;
			default:                  result += append_format(builder, "  Section number:        {} \"{}\"\n", symbol.SectionNumber, coff::get_name(reader.sections[symbol.SectionNumber-1].Name, reader)); break;
		}
		result += append_format(builder, "  Type:                  {}\n", symbol.Type);
		result += append_format(builder, "  Storage class:         {}\n", coff::storage_class_as_string(symbol.StorageClass));
		result += append_format(builder, "  Number of aux symbols: {}\n", symbol.NumberOfAuxSymbols);
	}

	return result;
}

}

namespace coff {

inline Optional<Reader> read(auto path) {
	Reader result;

	auto file = read_entire_file(path);
	if (!file.data)
		return {};

	auto header = (Header *)file.data;

    result.buffer = file;
	result.header = header;
	result.sections = {(Section *)(file.data + sizeof(Header)), header->NumberOfSections};
	result.symbols = {(Symbol *)(file.data + header->PointerToSymbolTable), header->NumberOfSymbols};
	result.string_table = (char *)result.symbols.end();

	return result;
}

inline void write(Writer &writer, auto path) {
	umm buffer_size = sizeof(Header) + writer.sections.count * sizeof(Section) + writer.symbols.count * sizeof(Symbol) + 4;
	for (auto section : writer.sections) {
		buffer_size += section.relocations.count * sizeof(Relocation);
		buffer_size += section.line_numbers.count * sizeof(LineNumber);
		buffer_size += section.data.count;
		if (section.name.count > 8)
			buffer_size += section.name.count + 1;
	}
	for (auto symbol : writer.symbols) {
		if (symbol.name.count > 8)
			buffer_size += symbol.name.count + 1;
	}

	auto buffer = (u8 *)temporary_allocator.allocate(buffer_size);

	auto header = (Header *)buffer;
	auto sections = (Section *)(buffer + sizeof(Header));
	auto symbols = (Symbol *)(sections + writer.sections.count);
	auto strings = (u8 *)(symbols + writer.symbols.count);
	auto next_string = strings + 4;

	header->Machine = writer.Machine;
	header->TimeDateStamp        = writer.TimeDateStamp;
	header->SizeOfOptionalHeader = writer.SizeOfOptionalHeader;
	header->Characteristics      = writer.Characteristics;
	header->NumberOfSections     = writer.sections.count;
	header->NumberOfSymbols      = writer.symbols.count;
	header->PointerToSymbolTable = (u8 *)symbols - buffer;

	auto set_name = [&](char (&d)[8], Span<utf8> s) {
		if (s.count > 8) {
			((u32 *)d)[0] = 0;
			((u32 *)d)[1] = next_string - strings;

			memcpy(next_string, s.data, s.count);
			next_string[s.count] = 0;

			next_string += s.count + 1;
		} else {
			memcpy(d, s.data, s.count);
			if (s.count != 8) {
				d[s.count] = 0;
			}
		}
	};

	for (umm i = 0; i < writer.sections.count; ++i) {
		auto &s = writer.sections[i];
		auto &d = sections[i];

		set_name(d.Name, s.name);
		d.Characteristics = s.Characteristics;
	}

	for (umm i = 0; i < writer.symbols.count; ++i) {
		auto &s = writer.symbols[i];
		auto &d = symbols[i];

		set_name(d.Name, s.name);
	}

	*(u32 *)strings = next_string - strings;

	auto cursor = next_string;
	for (umm i = 0; i < writer.sections.count; ++i) {
		auto &s = writer.sections[i];
		auto &d = sections[i];

		d.PointerToRawData = cursor - buffer;
		d.SizeOfRawData = s.data.count;

		memcpy(cursor, s.data.data, s.data.count);
		cursor += s.data.count;

		d.PointerToRelocations = cursor - buffer;
		d.NumberOfRelocations = s.relocations.count;

		for_each(s.relocations, [&](Writer::Relocation &relocation) {
			assert(relocation.symbol);
			*(Relocation *)cursor = {
				.VirtualAddress = relocation.offset,
				.SymbolTableIndex = (u32)index_of(writer.symbols, relocation.symbol),
				.Type = relocation.type,
			};
			cursor += sizeof(Relocation);
		});


		d.PointerToLinenumbers = cursor - buffer;
		d.NumberOfLinenumbers = s.line_numbers.count;

		memcpy(cursor, s.line_numbers.data, s.line_numbers.count * sizeof(LineNumber));
		cursor += s.line_numbers.count * sizeof(LineNumber);
	}


	for (umm i = 0; i < writer.symbols.count; ++i) {
		auto &s = writer.symbols[i];
		auto &d = symbols[i];

		d.Value = s.Value;
		d.SectionNumber =
				s.section == Writer::undefined_section ? IMAGE_SYM_UNDEFINED :
				s.section == Writer::absolute_section  ? IMAGE_SYM_ABSOLUTE:
				s.section == Writer::debug_section     ? IMAGE_SYM_DEBUG :
				(s16)index_of(writer.sections, s.section) + 1;
		d.Type = s.Type;
		d.StorageClass = s.StorageClass;
		d.NumberOfAuxSymbols = s.NumberOfAuxSymbols;
	}

	assert(cursor == buffer + buffer_size);

	write_entire_file(path, {buffer, buffer_size});
}

}
