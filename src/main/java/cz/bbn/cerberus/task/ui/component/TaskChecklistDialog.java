package cz.bbn.cerberus.task.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.task.dto.TaskCheckDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.List;

public class TaskChecklistDialog extends AppDialog {

    private final List<TaskCheckDto> taskCheckDtoList;
    private final SetChecklistAction setChecklistAction;
    private final VerticalLayout mainVerticalLayout = new VerticalLayout();

    private List<Binder<TaskCheckDto>> binderList;

    public TaskChecklistDialog(List<TaskCheckDto> taskCheckDtoList, SetChecklistAction setChecklistAction) {
        this.taskCheckDtoList = taskCheckDtoList;
        this.setChecklistAction = setChecklistAction;
        initComponent();
    }

    private void initComponent() {

        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setMargin(false);
        verticalLayout.setPadding(false);

        setTitle(Transl.get("Checkbox list"));
        verticalLayout.add(mainVerticalLayout);
        //LayoutUtils.setBigInfiniteColumnResponsiveSteps(mainVerticalLayout);
        generateComponent();

        Button add = VaadinComponents.getButton(VaadinIcon.PLUS.create());
        add.addClickListener(e -> {
            taskCheckDtoList.add(new TaskCheckDto());
            generateComponent();
        });
        verticalLayout.add(add);
        setContent(verticalLayout);

        addCloseButton();

        Button submitButton = VaadinComponents.getSubmitButton();
        submitButton.addClickListener(e -> submit());
        addSubmitButton(submitButton);
    }

    private void generateComponent() {
        binderList = new ArrayList<>();
        mainVerticalLayout.removeAll();
        if (taskCheckDtoList.isEmpty()) {
            taskCheckDtoList.add(new TaskCheckDto());
        }
        for (TaskCheckDto taskCheckDto : taskCheckDtoList) {
            Binder<TaskCheckDto> binder = new Binder<>();
            binder.setBean(taskCheckDto);
            HorizontalLayout horizontalLayout = new HorizontalLayout();
            TextField value = new TextField(Transl.get("Checkbox text"));
            value.setWidth("30em");
            value.setMaxLength(255);
            binder.forField(value).asRequired(TextValues.CANNOT_BE_EMPTY)
                    .bind(TaskCheckDto::getCheckboxName, TaskCheckDto::setCheckboxName);
            horizontalLayout.add(value);
            IntegerField integerField = new IntegerField(Transl.get("No. of days to finish"));
            integerField.setMax(100);
            binder.forField(integerField).bind(TaskCheckDto::getDaysToFinish, TaskCheckDto::setDaysToFinish);
            horizontalLayout.add(integerField);
            Button remove = VaadinComponents.getButton(VaadinIcon.MINUS.create());
            remove.getElement().getStyle().set("margin-top", "2em");
            remove.addClickListener(e -> {
                taskCheckDtoList.remove(taskCheckDto);
                generateComponent();
            });
            binderList.add(binder);
            horizontalLayout.add(remove);
            horizontalLayout.setVerticalComponentAlignment(FlexComponent.Alignment.CENTER);
            mainVerticalLayout.add(horizontalLayout);
        }
    }

    private void submit() {
        boolean isOk = true;
        List<TaskCheckDto> taskList = new ArrayList<>();
        for (Binder<TaskCheckDto> binder : binderList) {
            if (binder.validate().isOk()) {
                taskList.add(binder.getBean());
            } else {
                isOk = false;
            }
        }
        if (isOk) {
            setChecklistAction.setChecklist(taskList);
            this.close();
        }
    }

}
