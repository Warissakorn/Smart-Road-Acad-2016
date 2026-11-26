"""Build the Smart Road partial CUIX with AutoCAD 2016 revision metadata."""

import argparse
from io import BytesIO
from pathlib import Path
import re
import xml.etree.ElementTree as ET
from zipfile import ZipFile, ZipInfo

ROOT = Path(__file__).resolve().parents[1]
CONTENTS = ROOT / "SmartRoad.bundle" / "Contents"
RIBBON_FILE = CONTENTS / "SmartRoadRibbon.cuix"
REVISION = {"MajorVersion": "20", "MinorVersion": "1", "UserVersion": "1"}
XML_SCHEMA = {
    "xmlns:xsd": "http://www.w3.org/2001/XMLSchema",
    "xmlns:xsi": "http://www.w3.org/2001/XMLSchema-instance",
}
PANELS = {
    "Roads": ("SRCREATEROAD",),
    "Walk & Transit": ("SRWALK", "SRTRANSIT"),
    "Junctions": ("SRJUNCTION", "SRCONNECT"),
    "Edit & Report": ("SREDIT", "SRSECTION", "SRSTYLE", "SRQUANTITY", "SMARTROAD"),
}
EMPTY_SECTIONS = (
    "AcceleratorRoot", "DigitizerButtonRoot", "DoubleClickRoot", "ImageMenuRoot",
    "MouseButtonRoot", "OverrideRoot", "PopMenuRoot", "QuickAccessToolbarRoot",
    "QuickPropertiesRoot", "RolloverTooltipRoot", "ScreenMenuRoot", "TabletMenuRoot",
    "ToolbarRoot", "WorkspaceRoot",
)


def toolbar_commands():
    source = (ROOT / "Smart Road.lsp").read_text(encoding="ascii")
    commands = {}
    for label, help_text, macro, command, icon in re.findall(
        r'\("([^"]+)" "([^"]+)" "((?:\^C)*([A-Z][A-Z0-9]+)) " "(SmartRoad\w+)"\)', source
    ):
        if command in commands or not re.search(rf"^\(defun c:{command}\s", source, re.M | re.I):
            raise ValueError(f"Duplicate or undefined command: {command}")
        commands[command] = (label, help_text, macro, icon)
    ordered = [command for group in PANELS.values() for command in group]
    if len(ordered) != len(commands) or set(ordered) != commands.keys():
        raise ValueError("Ribbon panels must cover all Smart Road toolbar commands exactly once.")
    return commands


def xml_bytes(root):
    ET.indent(root)
    return ET.tostring(root, encoding="utf-8", xml_declaration=True)


def create_package():
    commands = toolbar_commands()
    header = ET.Element("CustSection", XML_SCHEMA)
    ET.SubElement(header, "FileVersion", MajorVersion="0", MinorVersion="6",
                  IncrementalVersion="1", UserVersion="1")
    header_body = ET.SubElement(header, "Header")
    ET.SubElement(header_body, "FileReferences")
    common = ET.SubElement(ET.SubElement(header_body, "CommonConfiguration"), "CommonItems")
    ET.SubElement(common, "ModifiedRev", REVISION)

    menu = ET.Element("MenuGroup", XML_SCHEMA, Name="SMARTROADRIBBON", DisplayName="Smart Road")
    macros = ET.SubElement(menu, "MacroGroup", Name="SMARTROAD_COMMANDS")
    ribbon = ET.Element("RibbonRoot")
    panels = ET.SubElement(ribbon, "RibbonPanelSourceCollection", XML_SCHEMA)
    tab = ET.SubElement(ET.SubElement(ribbon, "RibbonTabSourceCollection"), "RibbonTabSource",
                        UID="SR_TAB", Text="Smart Road", DefaultDisplay="AddToWorkspace",
                        WorkspaceBehavior="AddTabOnly")
    ET.SubElement(tab, "ModifiedRev", REVISION)
    ET.SubElement(tab, "Name").text = "Smart Road"

    images = {}
    for index, (panel_label, items) in enumerate(PANELS.items(), start=1):
        panel_id = f"SR_PANEL_{index}"
        panel = ET.SubElement(panels, "RibbonPanelSource", UID=panel_id, Text=panel_label,
                              HiddenInEditor="false")
        ET.SubElement(panel, "ModifiedRev", REVISION)
        ET.SubElement(panel, "Name").text = panel_label
        row = ET.SubElement(panel, "RibbonRow", UID=f"{panel_id}_ROW")
        ET.SubElement(row, "ModifiedRev", REVISION)
        panel_ref = ET.SubElement(tab, "RibbonPanelSourceReference", UID=f"{panel_id}_REF",
                                  PanelId=panel_id, ResizeStyle="Default")
        ET.SubElement(panel_ref, "ModifiedRev", REVISION)
        for command in items:
            label, help_text, macro, icon = commands[command]
            macro_id = f"SR_MACRO_{command}"
            definition = ET.SubElement(ET.SubElement(macros, "MenuMacro", UID=macro_id),
                                       "Macro", type="Any")
            ET.SubElement(definition, "Revision", REVISION)
            ET.SubElement(definition, "ModifiedRev", REVISION)
            ET.SubElement(definition, "Name").text = label
            ET.SubElement(definition, "Command").text = macro + ";"
            ET.SubElement(definition, "HelpString").text = help_text
            for tag, size in (("SmallImage", 16), ("LargeImage", 32)):
                filename = f"{icon}{size}.bmp"
                images[filename] = (CONTENTS / "Icons" / filename).read_bytes()
                ET.SubElement(definition, tag, Name=filename)
            ET.SubElement(definition, "CLICommand").text = command
            ET.SubElement(definition, "ToolTip")
            button = ET.SubElement(row, "RibbonCommandButton", UID=f"SR_BUTTON_{command}",
                                   Id="AcRibbonCommandButton", Text=label,
                                   ButtonStyle="LargeWithText", MenuMacroID=macro_id, KeyTip="")
            ET.SubElement(button, "TooltipTitle").text = help_text
            ET.SubElement(button, "ModifiedRev", REVISION)
        panel_break = ET.SubElement(panel, "RibbonPanelBreak", UID=f"{panel_id}_BREAK",
                                    Id="AcRibbonPanelBreak")
        ET.SubElement(panel_break, "ModifiedRev", REVISION)

    parts = {"Header.cui": header, "MenuGroup.cui": menu, "RibbonRoot.cui": ribbon}
    parts.update({f"{name}.cui": ET.Element(name, XML_SCHEMA) for name in EMPTY_SECTIONS})
    relationships = ET.Element("Relationships", xmlns="http://schemas.openxmlformats.org/package/2006/relationships")
    for index, name in enumerate((*parts, *images), start=1):
        ET.SubElement(relationships, "Relationship", Id=f"R{index:016x}", Target=f"/{name}",
                      Type="CUI" if name in parts else "Image")
    metadata = ET.Element("MenuPackageParts")
    for name in (*parts, "VirtualMNRRoot", "Menu_Package_Info.xml", *images):
        ET.SubElement(metadata, "PartData", PartData_Name=f"/{name}",
                      PartData_Modified="2026-11-25T00:00:00.0000000+00:00")
    content_types = ET.Element("Types", xmlns="http://schemas.openxmlformats.org/package/2006/content-types")
    for extension, mime in (
        ("cui", "text/xml"), ("xml", "text/xml"), ("bmp", "image/bmp"),
        ("rels", "application/vnd.openxmlformats-package.relationships+xml"),
    ):
        ET.SubElement(content_types, "Default", Extension=extension, ContentType=mime)
    parts.update({"[Content_Types].xml": content_types, "_rels/.rels": relationships,
                  "Menu_Package_Info.xml": metadata})

    result = BytesIO()
    with ZipFile(result, "w") as archive:
        for name, element in parts.items():
            archive.writestr(zip_entry(name), xml_bytes(element))
        for name, picture in images.items():
            archive.writestr(zip_entry(name), picture)
    return result.getvalue()


def zip_entry(name):
    entry = ZipInfo(name, (2026, 11, 25, 0, 0, 0))
    entry.create_system = 0
    return entry


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="Verify the bundled CUIX is current")
    options = parser.parse_args()
    generated = create_package()
    if options.check:
        if not RIBBON_FILE.is_file() or RIBBON_FILE.read_bytes() != generated:
            parser.exit(1, "Bundled CUIX is stale. Run python tools/build_ribbon.py.\n")
    else:
        RIBBON_FILE.write_bytes(generated)
