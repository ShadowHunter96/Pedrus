package cz.bbn.cerberus.commons.component.ui.appcard;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.html.H2;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.BackButtonDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ObjectWasChangedAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.SerializationUtils;

import java.io.Serializable;


public abstract class AppDetailCardComponent<T extends Serializable> extends AppCard {

    private final AppEnv appEnv;

    private final H2 heading = new H2();

    private final Binder<T> binder = new Binder<>();
    private final T dto;
    private final T originalDto;
    private final SaveAction<T> saveAction;
    private final boolean showSubmitButton;
    private ObjectWasChangedAction objectWasChangedAction;
    private Button saveButton;

    protected AppDetailCardComponent(
            T dto,
            SaveAction<T> saveAction,
            boolean showSubmitButton,
            AppEnv appEnv,
            EntityNewComponentOperation entityNewComponentOperation) {
        super(entityNewComponentOperation);
        this.appEnv = appEnv;
        this.dto = dto;
        this.saveAction = saveAction;
        this.showSubmitButton = showSubmitButton;
        originalDto = SerializationUtils.clone(dto);

        super.showFooter(true);
        super.addToContent(heading);
    }

    public void addLinkButton(String title, ComponentEventListener<ClickEvent<Button>> listener) {
        Button submit = VaadinComponents.getLinkButton(title);
        submit.addClickListener(listener);
        addButton(submit);
    }

    protected abstract void initComponent();

    public Binder<T> getBinder() {
        return binder;
    }

    public T getDto() {
        return dto;
    }

    public boolean isShowSubmitButton() {
        return showSubmitButton;
    }

    public T getOriginalDto() {
        return originalDto;
    }


    protected void setHeading(String heading) {
        this.heading.setText(heading);
    }

    protected void addSaveButton() {
        binder.setBean(getDto());
        saveButton = VaadinComponents.getSubmitButton();
        saveButton.setDisableOnClick(true);
        saveButton.addClickListener(buttonClickEvent -> {
            if (binder.validate().isOk()) {
                saveAction.saveItem(getDto(), getOriginalDto());
            } else {
                ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT), appEnv);
            }
            saveButton.setEnabled(true);
        });
        saveButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);

        saveButton.addClassName(RobotFrameworkVariables.SAVE_ITEM_BUTTON_CLASS.getValue());
        super.addToFooter(saveButton);
    }

    protected void addBackButton(String route) {
        Button backButton = VaadinComponents.getBackButton();

        backButton.addClickListener(buttonClickEvent -> {
                    boolean objectWasChanged = this.objectWasChangedAction != null ?
                            this.objectWasChangedAction.objectWasChanged() : !dto.equals(originalDto);
                    if (getSaveAction() != null && objectWasChanged) {
                        BackButtonDialog backButtonDialog = new BackButtonDialog();
                        backButtonDialog.open();
                    } else {
                        UI.getCurrent().navigate(route);
                    }
                }
        );
        super.addToFooter(backButton);
    }

    protected void addButton(Button... button) {
        super.addToFooter(button);
    }

    public SaveAction<T> getSaveAction() {
        return saveAction;
    }

    public void setObjectWasChangedAction(ObjectWasChangedAction objectWasChangedAction) {
        this.objectWasChangedAction = objectWasChangedAction;
    }

    public AppEnv getAppEnv() {
        return appEnv;
    }

    public void saveItem() {
        saveAction.saveItem(dto, originalDto);
    }
}
