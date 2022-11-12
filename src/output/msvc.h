#pragma once
#include <tl/string.h>
#include <tl/file.h>
static Span<utf8> locate_msvc() {
	Span<utf8> path = u8"C:\\Program Files (x86)\\Microsoft Visual Studio\\"s;

	auto find_version = [&]() -> Span<utf8> {
		Span<utf8> supported_versions[] = {
			u8"2022"s,
			u8"2019"s,
			u8"2017"s,
		};
		auto found_versions = get_items_in_directory(path);
		for (auto supported_version : supported_versions) {
			for (auto found_version : found_versions) {
				if (found_version.kind == FileItem_directory) {
					auto name = found_version.name;
					if (name == supported_version) {
						return format(u8"{}{}", path, name);
					}
				}
			}
		}
		return {};
	};
	auto find_edition = [&]() -> Span<utf8> {
		auto found_editions = get_items_in_directory(path);
		Span<utf8> supported_editions[] = {
			u8"Community"s,
			u8"Professional"s,
		};
		for (auto supported_edition : supported_editions) {
			for (auto found_edition : found_editions) {
				if (found_edition.kind == FileItem_directory) {
					auto name = found_edition.name;
					if (name == supported_edition) {
						return format(u8"{}\\{}\\VC\\Tools\\MSVC\\", path, name);
					}
				}
			}
		}
		return {};
	};


	path = find_version();
	if (!path.data) {
		return {};
	}
	path = find_edition();
	if (!path.data) {
		return {};
	}

	auto found_builds = get_items_in_directory(path);
	if (!found_builds.count) {
		return {};
	}
	path = format(u8"{}{}", path, found_builds.back().name);

	return path;
}

static Span<utf8> locate_wkits() {
	Span<utf8> path = u8"C:\\Program Files (x86)\\Windows Kits\\"s;

	auto find_version = [&]() -> Span<utf8> {
		Span<utf8> supported_versions[] = {
			u8"10"s,
			u8"8.1"s,
			u8"7"s,
		};
		auto found_versions = get_items_in_directory(path);
		for (auto supported_version : supported_versions) {
			for (auto found_version : found_versions) {
				if (found_version.kind == FileItem_directory) {
					auto name = found_version.name;
					if (name == supported_version) {
						return format(u8"{}{}\\Lib\\", path, name);
					}
				}
			}
		}
		return {};
	};

	path = find_version();
	if (!path.data) {
		return {};
	}

	auto found_builds = get_items_in_directory(path);
	if (!found_builds.count) {
		return {};
	}
	// NOTE: no slash at the end because link.exe does not understand that
	path = format(u8"{}{}\\um\\x64", path, found_builds.back().name);

	return path;
}
