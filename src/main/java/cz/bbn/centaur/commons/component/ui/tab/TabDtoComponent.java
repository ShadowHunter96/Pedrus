package cz.bbn.cerberus.commons.component.ui.tab;

import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.SerializationUtils;

import java.io.Serializable;

public abstract class TabDtoComponent<T extends Serializable> extends TabSimpleComponent {

    private final Binder<T> binder = new Binder<>();
    private final T dto;
    private final T originalDto;
    private final SaveAction<T> saveAction;
    private final AppEnv appEnv;

    protected TabDtoComponent(
            T dto,
            SaveAction<T> saveAction,
            AppEnv appEnv
    ) {
        this.dto = dto;
        originalDto = SerializationUtils.clone(dto);
        binder.setBean(getDto());
        this.saveAction = saveAction;
        this.appEnv = appEnv;
        getElement().getStyle().set("overflow", "auto");
    }

    protected abstract void initTab();

    public Binder<T> getBinder() {
        return binder;
    }


    public T getDto() {
        return dto;
    }

    public T getOriginalDto() {
        return originalDto;
    }

    @Override
    public void saveItem() {
        if (binder.validate().isOk()) {
            saveAction.saveItem(getDto(), getOriginalDto());
        } else {
            ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT), appEnv);
        }
    }

    public AppEnv getApptEnv() {
        return appEnv;
    }
}
