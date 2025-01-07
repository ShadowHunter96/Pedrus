package cz.bbn.cerberus.commons;

import lombok.Getter;

@Getter
public class AppGridStringVariables {

    private final String editTitle;
    private final String deleteConfirm;
    private String deleteWarningFirst = "";
    private String deleteWarningSecond = "";
    private String deleteWarningThird = "";
    private final String deleteTitle;
    private String item;

    public AppGridStringVariables(String editTitle, String deleteConfirm, String deleteWarningFirst,
                                  String deleteWarningSecond, String deleteWarningThird, String deleteTitle) {
        this.editTitle = editTitle;
        this.deleteConfirm = deleteConfirm;
        this.deleteWarningFirst = deleteWarningFirst;
        this.deleteWarningSecond = deleteWarningSecond;
        this.deleteWarningThird = deleteWarningThird;
        this.deleteTitle = deleteTitle;
    }

    public AppGridStringVariables(String editTitle, String deleteConfirm, String deleteTitle) {
        this.editTitle = editTitle;
        this.deleteConfirm = deleteConfirm;
        this.deleteTitle = deleteTitle;
    }

    public AppGridStringVariables(String item) {
        this.item = item;
        this.editTitle = null;
        this.deleteConfirm = null;
        this.deleteTitle = null;
    }
}
