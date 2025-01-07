package cz.bbn.cerberus.commons.component.ui.slidetab;

import com.vaadin.flow.component.html.Span;

public class CountIntIndicator extends Span {

    private int count;

    public CountIntIndicator(int count) {
        setText(String.valueOf(count));
        setClassName("new-slide-tab-count-span");
        this.count = count;
    }

    public void setCount(int count) {
        setText(String.valueOf(count));
        this.count = count;
    }

    public int getCount() {
        return count;
    }
}
