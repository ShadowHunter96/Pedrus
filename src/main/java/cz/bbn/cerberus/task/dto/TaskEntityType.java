package cz.bbn.cerberus.task.dto;

public enum TaskEntityType {

    TASK("New task", "Edit task"),
    TEMPLATE("New task template", "Edit task template"),
    SCHEDULE("New task schedule", "Edit task schedule");

    private final String newText;
    private final String editText;

    TaskEntityType(String newText, String editText) {
        this.newText = newText;
        this.editText = editText;
    }

    public String getNewText() {
        return newText;
    }

    public String getEditText() {
        return editText;
    }
}
