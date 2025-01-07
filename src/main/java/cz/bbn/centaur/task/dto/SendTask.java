package cz.bbn.cerberus.task.dto;

public enum SendTask {

    APP_NOTIFICATION("Application notification"), EMAIL("Email"), SMS("SMS");

    private final String name;

    SendTask(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
