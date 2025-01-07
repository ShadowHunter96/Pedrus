package cz.bbn.cerberus.document.ui.component;

public enum DocumentFileTypeEnum {

    PDF,
    PICTURE,
    CSV,
    TXT,
    WORD,
    XML,
    UNSUPPORTED;

    public static DocumentFileTypeEnum getTypeByExtension(String extension) {
        if ("pdf".equalsIgnoreCase(extension)) {
            return PDF;
        }
        if ("jpeg".equalsIgnoreCase(extension) || "jpg".equalsIgnoreCase(extension)
                || "png".equalsIgnoreCase(extension) || "gif".equalsIgnoreCase(extension)) {
            return PICTURE;
        }
        if ("csv".equalsIgnoreCase(extension)) {
            return CSV;
        }
        if ("txt".equalsIgnoreCase(extension) || "log".equalsIgnoreCase(extension)
                || "json".equalsIgnoreCase(extension) || "xml".equalsIgnoreCase(extension)) {
            return TXT;
        }
        return UNSUPPORTED;
    }
}
