package jdt;

import org.apache.commons.lang.StringUtils;

import java.util.ArrayList;
import java.util.List;

import java.lang.reflect.Method;

public class ListMembers {
    
    public static void main(String[] args) throws ClassNotFoundException {
        String className = args[0];
        Class cls = Class.forName(className);
        for (Method method : cls.getMethods()) {
            String name = method.getName();
            String returnType = method.getReturnType().getSimpleName();
            List<String> parameterTypes = new ArrayList<String>();
            for (Class paramClass : method.getParameterTypes()) {
                parameterTypes.add(paramClass.getSimpleName());
            }
            Test test;
            test.test();
            System.out.println(String.format("%s(%s): %s - %s",
                                             method.getName(),
                                             StringUtils.join(parameterTypes, ","),
                                             returnType,
                                             cls.getSimpleName()));
        }

    }

}